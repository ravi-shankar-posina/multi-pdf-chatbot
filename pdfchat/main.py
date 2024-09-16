import os
from flask import Flask, request, jsonify
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader, CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain.chains import RetrievalQA
from langchain_core.prompts import PromptTemplate
from langchain.agents.agent_types import AgentType
from langchain_experimental.agents.agent_toolkits import create_csv_agent
from dotenv import load_dotenv
from flask_cors import CORS

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

# Load CSV vector store
csv_retriever = load_csv_vector_store("./output.csv", "./StoreFAISSCSV")

# Load PDF vector store with multiple PDF files
pdf_files = [
    "./Reference/12.SD-Sales Monitoring and Analytics.pdf", 
    "./Reference/13.SD-Special Business Processes in Sales.pdf",
    "./Reference/14.SD-Integrations.pdf",
    "./Reference/SAP IM Database.pdf"
]
pdf_retriever = load_pdf_vector_store(pdf_files, "./StoreFAISSPDF")

# Create CSV agent
csv_agent = create_csv_agent(
    ChatOpenAI(temperature=0, model="gpt-4-1106-preview"),
    "./output.csv",
    verbose=True,
    agent_type=AgentType.OPENAI_FUNCTIONS,
    allow_dangerous_code=True
)

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

@app.route('/csv/agent_query', methods=['POST'])
def csv_agent_query():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    try:
        csv_response = csv_agent.run(query)
        return jsonify({"answer": csv_response, "type": "csv_agent"})
    except Exception as e:
        return jsonify({"error": str(e)}), 500

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

@app.route('/query', methods=['POST'])
def query():
    query = request.json.get("query", "")
    if not query:
        return jsonify({"error": "No query provided"}), 400

    response = openai_model.invoke(query)
    return jsonify({"answer": response.content})

if __name__ == '__main__':
    app.run(debug=True, port=8502, host="0.0.0.0")