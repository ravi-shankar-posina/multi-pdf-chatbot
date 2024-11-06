from PyPDF2 import PdfReader
from docx import Document
import os
import json
import io
import google.generativeai as genai
from langchain_google_genai import GoogleGenerativeAIEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.chains.question_answering import load_qa_chain
from langchain.prompts import PromptTemplate
from dotenv import load_dotenv
from pathlib import Path

# Load environment variables
load_dotenv()
genai.configure(api_key=os.getenv("GOOGLE_API_KEY"))

def read_pdf_from_directory(directory):
    text = ""
    directory_path = Path(directory)  # Convert string to Path object
    if not directory_path.exists():
        print(f"Warning: Directory {directory} does not exist")
        return text
        
    for filename in os.listdir(directory_path):
        if filename.endswith('.pdf'):
            pdf_path = directory_path / filename  # Use Path object for joining
            try:
                pdf_reader = PdfReader(pdf_path)
                for page in pdf_reader.pages:
                    text += page.extract_text() or ""
            except Exception as e:
                print(f"Error reading PDF {filename}: {e}")
    return text

def read_txt_from_directory(directory):
    text = ""
    directory_path = Path(directory)
    if not directory_path.exists():
        print(f"Warning: Directory {directory} does not exist")
        return text
        
    for filename in os.listdir(directory_path):
        if filename.endswith('.txt'):
            txt_path = directory_path / filename
            try:
                with open(txt_path, 'r', encoding='utf-8') as file:
                    text += file.read() + "\n"
            except Exception as e:
                print(f"Error reading text file {filename}: {e}")
    return text

def read_docx_from_directory(directory):
    text = ""
    directory_path = Path(directory)
    if not directory_path.exists():
        print(f"Warning: Directory {directory} does not exist")
        return text
        
    for filename in os.listdir(directory_path):
        if filename.endswith('.docx'):
            docx_path = directory_path / filename
            try:
                doc = Document(docx_path)
                for para in doc.paragraphs:
                    text += para.text + "\n"
            except Exception as e:
                print(f"Error reading DOCX {filename}: {e}")
    return text

def initialize_generative_model():
    return ChatGoogleGenerativeAI(model="gemini-1.5-flash", temperature=0.5)

def understand_tone_and_language(script_text):
    embeddings = GoogleGenerativeAIEmbeddings(model="models/embedding-001")
    vector_store = FAISS.from_texts([script_text], embedding=embeddings)
    vector_store.save_local("faiss_index")

def generate_script(user_input, generative_model, reference_context):
    embeddings = GoogleGenerativeAIEmbeddings(model="models/embedding-001")
    new_db = FAISS.load_local("faiss_index", embeddings, allow_dangerous_deserialization=True)
    script_docs = new_db.similarity_search(user_input)

    prompt = f"""
    Take the persona of a Test Script Generator for different kinds of SAP modules.
    In the context, test scripts are given as a reference.
    based on the structure in the references generate a new test script using the context and test case
    Generate a test script strictly based on the test case and purpose/overview of the context 
    For Fiori Codes based on the test script generated understand it and return the Fiori Codes from the context
    Strictly dont use pre-existing test scripts as a response for a test case 
    Do not generate the same test script for two different test cases
    For Example the Structure of the Test Script 

         {{
    "Intracompany STO": {{
        "Step Description": {{
            "1": "Login to SAP Fiori...",
            "2": "Access the App...",
            ...
        }},
        "Expected Results": {{
            "1": "SAP Fiori Launchpad is displayed",
            "2": "Purchase Order for STO...",
            ...
        }},
        "Fiori ID":"F0842A"
    }}
    }}
    Output Should be strictly in the form of json
    
    Reference Context: \n{reference_context}\n
    User Input: \n{user_input}\n
    """

    chain = load_qa_chain(generative_model, chain_type="refine")
    input_data = {
        "question": prompt,
        "input_documents": script_docs,
    }

    response = chain.invoke(input_data)
    output_text = response.get("output_text", "").strip()
    if output_text.startswith("```json") and output_text.endswith("```"):
        output_text = output_text[7:-3].strip()
    return json.loads(output_text)

def script(uploaded_text, case_description):
    # Define references directory using Path
    references_directory = Path("References")  # or Path("./References")
    
    # Check if directory exists
    if not references_directory.exists():
        print(f"Creating References directory at {references_directory}")
        references_directory.mkdir(parents=True, exist_ok=True)
    
    # Define paths for JSON files
    combined_steps_file = references_directory / "combined_steps.json"
    fiori_codes_file = references_directory / "Fioricodes.json"

    # Load JSON files with error handling
    try:
        with open(combined_steps_file, 'r') as file:
            reference_context = json.load(file)
    except FileNotFoundError:
        print(f"Warning: {combined_steps_file} not found")
        reference_context = {}
    except json.JSONDecodeError:
        print(f"Error: Invalid JSON in {combined_steps_file}")
        reference_context = {}

    try:
        with open(fiori_codes_file, 'r') as file:
            fiori_codes_context = json.load(file)
    except FileNotFoundError:
        print(f"Warning: {fiori_codes_file} not found")
        fiori_codes_context = {}
    except json.JSONDecodeError:
        print(f"Error: Invalid JSON in {fiori_codes_file}")
        fiori_codes_context = {}

    # Merge contexts
    reference_context.update(fiori_codes_context)

    # Read documents from directory using proper path
    reference_text = read_pdf_from_directory(references_directory)
    reference_text += read_txt_from_directory(references_directory)
    reference_text += read_docx_from_directory(references_directory)

    if reference_text:
        understand_tone_and_language(reference_text)
        
    if uploaded_text and case_description:
        generative_model = initialize_generative_model()
        understand_tone_and_language(uploaded_text + reference_text)
        test_scripts = {}
        test_scripts[case_description] = generate_script(case_description, generative_model, reference_context)
        return test_scripts[case_description]
    
    return {}