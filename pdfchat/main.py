import os
from flask import Flask, request, jsonify
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader, CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain.chains import RetrievalQA
from langchain_core.prompts import PromptTemplate
from langchain.agents.agent_types import AgentType
from dotenv import load_dotenv
from flask_cors import CORS
from pandasai import SmartDataframe
import pandas as pd
from pandasai.llm.openai import OpenAI

# Load environment variables
load_dotenv()

# Initialize Flask app
app = Flask(__name__)
CORS(app, origins="*")

# Initialize OpenAI models
embeddings = OpenAIEmbeddings(model="text-embedding-3-large")
openai_model = ChatOpenAI(model="gpt-4o-mini")

def load_csv_vector_store(file_path, vectordb_path):
    """
    Load or create a vector store for the CSV file.
    """
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
    """
    Load or create a vector store for multiple PDF files.
    """
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

csv_retriever = load_csv_vector_store("./HowToDataStore.csv", "./StoreFAISSCSV")

pdf_files = [
    "./Reference/12.SD-Sales Monitoring and Analytics.pdf", 
    "./Reference/13.SD-Special Business Processes in Sales.pdf",
    "./Reference/14.SD-Integrations.pdf",
    "./Reference/SAP IM Database.pdf"
]
pdf_retriever = load_pdf_vector_store(pdf_files, "./StoreFAISSPDF")

def serialize_document(doc):
    """
    Serialize a document object for JSON response.
    """
    return {
        "page_content": str(doc.page_content),
        "metadata": {
            "source": str(doc.metadata.get("source", "")),
            "page": str(doc.metadata.get("page", 0))
        }
    }

def create_chain(retriever, prompt_template):
    """
    Create a retrieval chain with the given retriever and prompt template.
    """
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

# Define prompt templates
csv_prompt = """
You are an assistant for generating responses for CSV-based queries. Use the following context extracted from CSV data to answer the question:

Context: {context}

Question: {question}

If the answer is not in the context, say "Below details might help you." and return the summary of content of the source document.
"""

pdf_prompt = """
You are a PDF Reader AI Assistant. Your task is to take in the user query and generate the answer only from the context provided. 

Context: {context}

Question: {question}
"""

# Create retrieval chains
csv_chain = create_chain(csv_retriever, csv_prompt)
pdf_chain = create_chain(pdf_retriever, pdf_prompt)

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

@app.route("/analyze", methods=["POST"])
def analyze_excel():
    query = request.json.get("query", "")
    
    # Initialize OpenAI and SmartDataframe
    llm = OpenAI(api_token=os.getenv("OPENAI_API_KEY"))
    sdf = SmartDataframe('./output.csv', config={"llm": llm})
    
    # Get the response from SmartDataframe
    response = sdf.chat(query)
    
    # Handle response based on its type
    if isinstance(response, (int, str)):
        response_data = str(response)  # Convert int or str to string
    elif isinstance(response, pd.DataFrame):
        response_data = response.to_html()  # Convert DataFrame to JSON string
    else:
        response_data = str(response)  # Fallback for other types

    return jsonify({"answer": response_data})

@app.route('/query', methods=['POST'])
def query():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = openai_model.invoke(query)
    return jsonify({"answer": response.content})

if __name__ == '__main__':
    app.run(debug=True, port=8502, host="0.0.0.0")