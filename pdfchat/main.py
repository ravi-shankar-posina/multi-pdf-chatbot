import os
import tempfile
import zipfile
from flask import Flask, request, jsonify, send_file
from werkzeug.utils import secure_filename
from PyPDF2 import PdfReader
from docx import Document
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader, CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain.chains.question_answering import  load_qa_chain
from langchain.chains import RetrievalQA
from langchain_core.prompts import PromptTemplate
from langchain.agents.agent_types import AgentType
from dotenv import load_dotenv
from flask_cors import CORS
from pandasai import SmartDataframe
import pandas as pd
from pandasai.llm.openai import OpenAI
from abap import modify_abap_code
from testscripts import script
import json
from dbconn import *
from models import IDoc, MasterData, POAutomation,Apsuite

# Load environment variables
load_dotenv()

# Initialize Flask app
app = Flask(__name__)
CORS(app, origins="*")
app.config['UPLOAD_FOLDER'] = 'uploads'
app.config['MAX_CONTENT_LENGTH'] = 16 * 1024 * 1024  # 16MB max file size
# Specify encoding to handle non-UTF-8 files
encoding = "latin1"

# Ensure upload folder exists
os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)

# Initialize OpenAI models
embeddings = OpenAIEmbeddings(model="text-embedding-3-large")
openai_model = ChatOpenAI(model="gpt-4o-mini")
generative_model = ChatOpenAI(model="gpt-4", temperature=0.5)

# File Reading Functions
def read_pdf(pdf_file):
    text = ""
    pdf_reader = PdfReader(pdf_file)
    for page in pdf_reader.pages:
        text += page.extract_text()
    return text

def read_txt(txt_file):
    return txt_file.read().decode("utf-8")

def read_docx(docx_file):
    text = ""
    doc = Document(docx_file)
    for para in doc.paragraphs:
        text += para.text + "\n"
    return text

# Vector Store Functions
def load_csv_vector_store(file_path, vectordb_path):
    """Load or create a vector store for the CSV file."""
    loader = CSVLoader(file_path=file_path, encoding='latin-1')
    data = loader.load()
    text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = text_splitter.split_documents(data)

    if os.path.exists(vectordb_path):
        vectordb = FAISS.load_local(vectordb_path, embeddings=embeddings, allow_dangerous_deserialization=True)
        print(f"Loaded existing FAISS index from {vectordb_path}")
    else:
        vectordb = FAISS.from_documents(documents=split_docs, embedding=embeddings)
        vectordb.save_local(folder_path=vectordb_path)
        print(f"Created and saved new FAISS index to {vectordb_path}")

    return vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})

def load_pdf_vector_store(file_paths, vectordb_path):
    """Load or create a vector store for multiple PDF files."""
    all_docs = []
    for file_path in file_paths:
        loader = PyPDFLoader(file_path=file_path)
        documents = loader.load()
        all_docs.extend(documents)

    text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = text_splitter.split_documents(all_docs)
    
    if os.path.exists(vectordb_path):
        vectordb = FAISS.load_local(vectordb_path, embeddings=embeddings, allow_dangerous_deserialization=True)
        print(f"Loaded existing FAISS index from {vectordb_path}")
    else:
        vectordb = FAISS.from_documents(documents=split_docs, embedding=embeddings)
        vectordb.save_local(folder_path=vectordb_path)
        print(f"Created and saved new FAISS index to {vectordb_path}")

    return vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})

# Initialize vector stores and commented on 08/06/2025
# Define the combined CSV path
combined_csv_path = "./DataDump_ofServiceRequests.csv"

# Create combined CSV file - Added on 08/06/2025
pd.concat([
    pd.read_csv("./change_requests_last_12_months.csv", encoding=encoding),
    pd.read_csv("./open_incidents_as_on_13thMay.csv", encoding=encoding),
    pd.read_csv("./resolved_incidents_last_12_months.csv", encoding=encoding),
]).to_csv(combined_csv_path, index=False)

csv_retriever = load_csv_vector_store(combined_csv_path, "./StoreFAISSCSV")

# pdf_files = [
#     "./Reference/12.SD-Sales Monitoring and Analytics.pdf", 
#     "./Reference/13.SD-Special Business Processes in Sales.pdf",
#     "./Reference/14.SD-Integrations.pdf",
#     "./Reference/SAP IM Database.pdf"
# ]
pdf_files = [
    "./Reference/Integrations.pdf", 
    "./Reference/Sales Monitoring and Analytics.pdf",
    "./Reference/Special Business Processes in Sales.pdf",
]
kt_files = ["./Reference/ktFiles/E2E_ProcessKT_OfEmpowerRebate.pdf",
            "./Reference/ktFiles/Fix_SuspendedInvoices_KTSession.pdf",
            "./Reference/ktFiles/Knowledge_Transfer_documentNA_BOM_interfaceImpDetails.pdf"]
pdf_retriever = load_pdf_vector_store(pdf_files, "./StoreFAISSPDF")
kt_retriever =  load_pdf_vector_store(kt_files, "./StoreFAISSKT")

# Helper Functions
def serialize_document(doc):
    """Serialize a document object for JSON response."""
    return {
        "page_content": str(doc.page_content),
        "metadata": {
            "source": str(doc.metadata.get("source", "")),
            "page": str(doc.metadata.get("page", 0))
        }
    }

def create_chain(retriever, prompt_template):
    """Create a retrieval chain with the given retriever and prompt template."""
    prompt = PromptTemplate(
        template=prompt_template, 
        input_variables=["context", "question"]
    )
    
    return RetrievalQA.from_chain_type(
        llm=ChatOpenAI(model="gpt-4-1106-preview"),
        chain_type="stuff",
        retriever=retriever,
        return_source_documents=True,
        chain_type_kwargs={"prompt": prompt}
    )

# Test Case Generation Functions
def understand_tone_and_language(script_text):
    vector_store = FAISS.from_texts([script_text], embedding=embeddings)
    vector_store.save_local("faiss_index")

def generate_script(user_input, generative_model):
    new_db = FAISS.load_local("faiss_index", embeddings, allow_dangerous_deserialization=True)
    script_docs = new_db.similarity_search(user_input)

    prompt_template = """
    Take the persona of AI SAP Test Case Generator.
    A Testcase is an end to end workflow but not a single step process in SAP
    In the context PDD overviews along with their test cases are given to train the model.
    Extract the overview/purpose of the input provided.
    For the overview extracted generate the test case/s based on the above patterns of PDD overviews and Test Cases and return the text case/s as a response.
    Generate me strictly the test cases only, not test scripts or test Descriptions.
    Do not consider validations or verifications as Test Cases
    If there are any test cases already in the reference do not use them generate them based on the purpose/overview
    Also make sure that the test case does not exceed 10 words.
    The output should be in such a way that each test case should be printed in separate lines

    Context: \n{context}\n
    Generated Test Cases:
    """
    
    prompt = PromptTemplate(template=prompt_template, input_variables=["context"])
    chain = load_qa_chain(generative_model, chain_type="stuff", prompt=prompt)
    response = chain.invoke({"input_documents": script_docs}, return_only_outputs=True)

    # print(response["output_text"])
    return response["output_text"]

def test_cases_to_excel(test_cases):
    # print(test_cases)
    data = []
    
    # Iterate over each line in the test cases input
    for case in test_cases.splitlines():
        # Check if the case starts with a number followed by a period
        if case.lstrip().startswith(str(case.lstrip()[0]) + "."):
            # Split the string at the first period
            parts = case.split(".", 1)
            test_case_number = parts[0].strip()  # Clean the test case number
            description = parts[1].strip()       # Clean the description
            
            # Append the extracted information to the data list
            data.append([test_case_number, description])
    
    # Return the data as a pandas DataFrame
    return pd.DataFrame(data, columns=["Test Case Number", "Description"])

def convert_to_excel(test_scripts, file_path):
    # print("Test Scripts Structure:", test_scripts)  # Debugging step
    with pd.ExcelWriter(file_path, engine='xlsxwriter') as writer:
        workbook = writer.book
        header_format = workbook.add_format({'bold': True, 'bg_color': '#FFD700'})
        title_format = workbook.add_format({'bold': True, 'bg_color': '#ADD8E6'})

        # Create summary sheet
        summary_sheet_name = "Test Cases"
        summary_data = []

        for i, (case_title, script_details) in enumerate(test_scripts.items(), start=1):
            steps = []
            for scenario, details in script_details.items():
                fiori_id = details.get("Fiori ID", "")
                step_desc = details.get("Step Description", {})
                exp_results = details.get("Expected Results", {})

                for step_num, step_desc_text in step_desc.items():
                    steps.append({
                        "Step Number": step_num,
                        "Step Description": step_desc_text,
                        "Expected Result": exp_results.get(step_num, ""),
                        "Fiori ID": fiori_id,
                    })

            df = pd.DataFrame(steps)
            sheet_name = f"TestScript {i}"
            df.to_excel(writer, index=False, sheet_name=sheet_name, startrow=1)

            worksheet = writer.sheets[sheet_name]
            worksheet.write(0, 0, case_title, title_format)
            summary_data.append([f"Test Case {i}: {case_title}"])

            for col_num, value in enumerate(df.columns.values):
                worksheet.write(1, col_num, value, header_format)

        summary_df = pd.DataFrame(summary_data, columns=["Test Case Overview"])
        summary_df.to_excel(writer, index=False, sheet_name=summary_sheet_name, startrow=0)
        summary_worksheet = writer.sheets[summary_sheet_name]
        summary_worksheet.set_column(0, 0, 50)
        summary_worksheet.write(0, 0, "Test Case Overview", header_format)

# Define prompt templates
csv_prompt = """
You are an assistant for generating the responses for CSV prompts. 
Use the following pieces of retrieved context to answer the question. 
If the answer is not present, say: "I don't know." 
Provide a full, detailed answer in the following markdown format:

# **Main Heading (Query Title)**

**Step-1**

Provide the first step in plain text here.

**Step-2**

Provide the second step in plain text here.

**Step-3**

Continue for additional steps as required, each with the same formatting.

### **Additional Information**

If there are additional notes, include them here in plain text.

{context}
"""

pdf_prompt = """
You are a PDF Reader AI Assistant. Your task is to analyze the user query and provide answers strictly based on the provided context.

1. Carefully examine the context for information, including any tabular data.
2. If tables are present, extract and include their relevant portions in your response as structured text.
3. Avoid adding interpretations or explanations not directly supported by the context.
4. If you cannot find an answer to the query within the provided context, respond with: "The answer is not available in the provided context." Do not speculate or provide incomplete information.
CONTEXT: {context}
QUESTION: {question}
"""
# Create retrieval chains
csv_chain = create_chain(csv_retriever, csv_prompt)
pdf_chain = create_chain(pdf_retriever, pdf_prompt)
kt_chain = create_chain(kt_retriever, pdf_prompt)

# Routes
@app.route('/idoc-issues', methods=['GET'])
def get_idocs():
    try:
        # Get total count for statistics
        total_count = IDoc.objects().count()
        
        # Fetch only failed IDoc records (status_code = 51)
        failed_idocs = IDoc.objects(status_code=51)
        idoc_list = []
        
        for doc in failed_idocs:
            item = doc.to_mongo().to_dict()
            item["_id"] = str(item["_id"])  # Convert ObjectId to string
            item["status_category"] = "Failed"
            idoc_list.append(item)
        
        # Calculate counts
        failure_count = len(idoc_list)
        success_count = total_count - failure_count
        
        # Prepare response with failed records and counts
        response_data = {
            "total_records": total_count,
            "success_count": success_count,
            "failure_count": failure_count,
            "failed_records": idoc_list  # Only failed records in the data
        }
        
        return jsonify(response_data), 200
        
    except Exception as e:
        return jsonify({"error": f"Failed to fetch IDoc records: {str(e)}"}), 500
@app.route('/po-data', methods=['GET'])
def get_po_data():
    try:
        po_Data = POAutomation.objects()
        po_list = []

        for doc in po_Data:
            doc_dict = doc.to_mongo().to_dict()
            if "_id" in doc_dict:
                doc_dict["_id"] = str(doc_dict["_id"])  # convert ObjectId to string
            po_list.append(doc_dict)

        return jsonify(po_list), 200
    except Exception as e:
        return jsonify({"error": f"Failed to fetch PO records: {str(e)}"}), 500
@app.route('/apsuite-data', methods=['POST'])
def get_apsuite_data():
    try:
        # Get the JSON data from request body
        data = request.get_json()
        
        if not data or 'mappingData' not in data:
            return jsonify({'error': 'Invalid mapping data provided'}), 400
        
        mapping_data = data['mappingData']
        
        if not isinstance(mapping_data, list):
            return jsonify({'error': 'Mapping data must be an array'}), 400
        
        if len(mapping_data) == 0:
            return jsonify({'error': 'No mapping data provided'}), 400
        
        # Create query conditions for filtering
        query_filter = []
        
        for item in mapping_data:
            if 'entityType' in item and 'typeOfData' in item and 'apsuiteName' in item:
                query_filter.append({
                    'entityType': item['entityType'],
                    'typeOfData': item['typeOfData'],
                    'apsuiteName': item['apsuiteName']
                })
        
        if not query_filter:
            return jsonify({'error': 'No valid mapping conditions found'}), 400
        
        # Get all Apsuite data
        apsuite_data = Apsuite.objects()
        apsuite_list = []

        for doc in apsuite_data:
            doc_dict = doc.to_mongo().to_dict()
            if "_id" in doc_dict:
                doc_dict["_id"] = str(doc_dict["_id"])  # convert ObjectId to string
            
            # Check if this document matches any of our filter conditions
            for filter_condition in query_filter:
                if (doc_dict.get('entityType') == filter_condition['entityType'] and 
                    doc_dict.get('typeOfData') == filter_condition['typeOfData'] and 
                    doc_dict.get('apsuiteName') == filter_condition['apsuiteName']):
                    apsuite_list.append(doc_dict)
                    break  # Avoid duplicate entries

        return jsonify(apsuite_list), 200
        
    except Exception as e:
        return jsonify({"error": f"Failed to fetch Apsuite records: {str(e)}"}), 500
    
@app.route('/idoc-update', methods=['POST'])
def update_idoc_status():
    try:
        # Find all IDoc records with status_code = 51
        failed_idocs = IDoc.objects(status_code=51)
        
        if not failed_idocs:
            return jsonify({
                "success": True,
                "message": "No IDoc records found with status_code 51",
                "updated_count": 0,
                "results": []
            }), 200
        
        # Store results for UI display
        results = []
        updated_count = 0
        
        for idoc in failed_idocs:
            # Find matching master data by routine_function_mode
            master_data = None
            if idoc.routine_function_mode:
                master_data = MasterData.objects(routine_function_code=idoc.routine_function_mode).first()
            
            # Store original values
            original_data = {
                'order_number': idoc.order_number,
                'sales_org': idoc.sales_org,
                'sales_area': idoc.sales_area,
                'division': idoc.division,
                'amount': idoc.amount
            }
            
            # Initialize field updates tracking
            field_updates = {}
            
            if master_data:
                # Compare and update fields
                fields_to_check = ['order_number', 'sales_org', 'sales_area', 'division', 'amount']
                
                for field in fields_to_check:
                    idoc_value = getattr(idoc, field)
                    master_value = getattr(master_data, field)
                    
                    if idoc_value != master_value:
                        field_updates[field] = {
                            'old_value': idoc_value,
                            'new_value': master_value,
                            'status': 'updated'
                        }
                        # Update the IDoc field
                        setattr(idoc, field, master_value)
                    else:
                        field_updates[field] = {
                            'old_value': idoc_value,
                            'new_value': idoc_value,
                            'status': 'matched'
                        }
            else:
                # No master data found
                fields_to_check = ['order_number', 'sales_org', 'sales_area', 'division', 'amount']
                for field in fields_to_check:
                    field_value = getattr(idoc, field)
                    field_updates[field] = {
                        'old_value': field_value,
                        'new_value': field_value,
                        'status': 'no_master_data'
                    }
            
            # Update status_code and status_text
            idoc.status_code = 64
            idoc.status_text = "IDOC Status Updated - Processed"
            
            # Save the updated IDoc
            idoc.save()
            updated_count += 1
            
            # Prepare result for React UI in expected format
            updates = []
            
            field_mapping = {
                'order_number': 'Order Number',
                'sales_org': 'Sales Organization', 
                'sales_area': 'Sales Area',
                'division': 'Division',
                'amount': 'Amount'
            }
            
            for field_key, field_label in field_mapping.items():
                field_data = field_updates[field_key]
                update_item = {
                    'field': field_label,
                    'value': str(field_data['new_value']),
                    'status': 'matched' if field_data['status'] == 'matched' else 'updated'
                }
                
                # Add oldValue only if status is updated
                if field_data['status'] == 'updated':
                    update_item['oldValue'] = str(field_data['old_value'])
                
                updates.append(update_item)
            
            results.append({
                'idocNumber': str(idoc.idoc_number),
                'updates': updates
            })
        
        return jsonify({
            "success": True,
            "message": f"Successfully updated {updated_count} IDoc records from status_code 51 to 64",
            "updated_count": updated_count,
            "results": results
        }), 200
        
    except Exception as e:
        return jsonify({
            "success": False,
            "error": f"Failed to update IDoc records: {str(e)}",
            "updated_count": 0,
            "results": []
        }), 500
@app.route('/idoc-data', methods=['GET'])
def get_idoc_data():
    try:
        idocs = IDoc.objects()
        idoc_list = [json.loads(doc.to_json()) for doc in idocs]
        return jsonify(idoc_list), 200
    except Exception as e:
        return jsonify({"error": f"Failed to fetch IDoc records: {str(e)}"}), 500
@app.route('/csv/query', methods=['POST'])
def query_csv():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = csv_chain.invoke({"query": query})
    serializable_sources = [serialize_document(doc) for doc in response.get("source_documents", [])]

    return jsonify({
        "answer": str(response["result"]),
        "sources": serializable_sources
    })

@app.route('/pdf/query', methods=['POST'])
def query_pdf():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = pdf_chain.invoke({"query": query})
    serializable_sources = [serialize_document(doc) for doc in response.get("source_documents", [])]

    return jsonify({
        "answer": response["result"],
        "sources": serializable_sources
    })

@app.route("/kt/query", methods=["POST"])
def query_kt():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = kt_chain.invoke({"query": query})
    serializable_sources = [serialize_document(doc) for doc in response.get("source_documents", [])]

    return jsonify({
        "answer": response["result"],
        "sources": serializable_sources
    })
@app.route("/analyze", methods=["POST"])
def analyze_excel():
    try:
        # Extract the query from the request body
        query = request.json.get("query", "").strip()
        if not query:
            return jsonify({"error": "The query parameter is missing. Please provide a valid query."}), 400

        # Define the prompt to guide the LLM
        prompt_template = (
           "You are an advanced assistant for generating responses related to incidents, CSV data analysis, and graphs. "
            "Either the user may provide incidents in short descriptions or in long descriptions, you need to provide resolution for the same."
            "In either cases, provide the response from the csv file."
            "Use the following pieces of retrieved context to answer "
            "the question. If the answer is not present, say that: I don't know."
            "If the query includes the word 'compare', execute both the CSV agent and LIDA graph generation. The CSV agent provides tabular analysis, and LIDA generates graphs.\n"
            "If the query does not contain 'compare', determine if it is related to CSV analysis, graph generation, or retrieval, and respond accordingly.\n"
            "Provide a full, detailed answer"
            "Here is the query: {query}"
        )
        prompt = prompt_template.format(query=query)

        # Initialize OpenAI and SmartDataframe
        llm = OpenAI(api_token=os.getenv("OPENAI_API_KEY"))
        sdf = SmartDataframe('./Data_dumpOf_incidents.csv', config={"llm": llm})

        # Process the query using SmartDataframe with the prompt
        try:
            response = sdf.chat(prompt)
        except Exception as chat_error:
            return jsonify({
                "error": f"Failed to process the query. Please refine your question. Details: {str(chat_error)}"
            }), 400

        # Format the response based on its type
        if isinstance(response, pd.DataFrame):
            # Convert DataFrame to HTML
            response_data = response.to_html(index=False)
        elif isinstance(response, list):
            # Format list as a comma-separated string
            response_data = ", ".join(map(str, response))
        else:
            # Handle other types (int, str, float)
            response_data = str(response)

        # Return the response
        return jsonify({"answer": response_data})

    except FileNotFoundError:
        return jsonify({"error": "The file './Data_dumpOf_incidents.csv' could not be found."}), 404
    except Exception as e:
        # Catch unexpected errors and log them
        return jsonify({"error": f"An unexpected error occurred: {str(e)}"}), 500
@app.route('/query', methods=['POST'])
def query():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = openai_model.invoke(query)
    return jsonify({"answer": response.content})

@app.route('/modify-abap', methods=['POST'])
def modify_abap():
    query = request.json.get("query", "").strip()
    file_query, modification_request = query.split(',', 1)
    
    result = modify_abap_code(file_query.strip(), modification_request.strip())
    return jsonify({"answer": str(result)})
@app.route('/upload-file', methods=['POST'])
def upload_file():
    if 'file' not in request.files:
        return jsonify({'error': 'No file part'}), 400
    
    file = request.files['file']
    if file.filename == '':
        return jsonify({'error': 'No selected file'}), 400

    if file:
        filename = secure_filename(file.filename)
        file_path = os.path.join(app.config['UPLOAD_FOLDER'], filename)
        file.save(file_path)

        try:
            uploaded_text = ""
            if filename.endswith('.pdf'):
                with open(file_path, 'rb') as f:
                    uploaded_text = read_pdf(f)
            elif filename.endswith('.txt'):
                with open(file_path, 'rb') as f:
                    uploaded_text = read_txt(f)
            elif filename.endswith('.docx'):
                uploaded_text = read_docx(file_path)
            
            # Generate test cases
            understand_tone_and_language(uploaded_text)
            response = generate_script(uploaded_text, generative_model)
            
            # Ensure response has the expected structure
            # print("Response structure:", response)
            
            # Convert to DataFrame
            df = test_cases_to_excel(response)
            # print("DataFrame before saving to Excel:", df)
            
            # Generate test script details
            testscript = {desc: script(uploaded_text, desc) for desc in df["Description"]}
            # print("Testscript:", testscript)  # Debugging step
            
            # Save Excel file
            excel_filename = f"{os.path.splitext(filename)[0]}.xlsx"
            excel_path = os.path.join(app.config['UPLOAD_FOLDER'], excel_filename)
            convert_to_excel(testscript, excel_path)
            
            return send_file(
                excel_path,
                mimetype='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                as_attachment=True,
                download_name=excel_filename
            )

        except Exception as e:
            return jsonify({'error': f"Exception: {str(e)}"}), 500
        finally:
            if os.path.exists(file_path):
                os.remove(file_path)

    return jsonify({'error': 'Invalid file type'}), 400
@app.route('/upload-zip', methods=['POST'])
def upload_zip():
    if 'file' not in request.files:
        return jsonify({'error': 'No file part'}), 400
    
    file = request.files['file']
    if file.filename == '' or not file.filename.endswith('.zip'):
        return jsonify({'error': 'Invalid file'}), 400

    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            zip_path = os.path.join(temp_dir, 'upload.zip')
            file.save(zip_path)
            
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(temp_dir)
            
            # Process each file in the zip
            results = []
            for root, _, files in os.walk(temp_dir):
                for filename in files:
                    if filename.endswith(('.pdf', '.txt', '.docx')):
                        file_path = os.path.join(root, filename)
                        # Process file (similar to single file upload)
                        # Add results to the list
                        results.append({'filename': filename, 'status': 'processed'})

            return jsonify({'results': results})

    except Exception as e:
        return jsonify({'error': str(e)}), 500
@app.route('/chat', methods=['POST'])
def chat():
    data = request.json
    chat_history = data.get("chat_history", [])
    query = data.get("query", "")

    if not query:
        return jsonify({"error": "No query provided"}), 400

    formatted_history = [{"role": msg["role"], "content": msg["content"]} for msg in chat_history]

    try:
        response = openai_model.invoke(formatted_history + [{"role": "user", "content": query}])
        chat_history.append({"role": "user", "content": query})
        chat_history.append({"role": "assistant", "content": response.content})

        return jsonify({"answer": response.content, "chat_history": chat_history})
    except Exception as e:
        return jsonify({"error": str(e)}), 500
if __name__ == '__main__':
    app.run(debug=True, port=8502, host="0.0.0.0")