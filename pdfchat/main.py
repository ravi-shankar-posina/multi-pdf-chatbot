import os
import fitz
from flask import Flask, request, jsonify
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader, CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain.chains import RetrievalQA, create_retrieval_chain
from langchain.chains.combine_documents import create_stuff_documents_chain
from langchain_core.prompts import ChatPromptTemplate, PromptTemplate
from langchain_core.output_parsers import StrOutputParser
from dotenv import load_dotenv
from flask_cors import CORS
import base64
load_dotenv()
app = Flask(__name__)
CORS(app, origins="*")

# Common embeddings setup
embeddings = OpenAIEmbeddings(model="text-embedding-3-large")
openai_model = ChatOpenAI(model="gpt-4")
parser = StrOutputParser()

# Load CSV data
def load_csv_data(file_path):
    loader = CSVLoader(file_path=file_path, encoding='latin-1')
    data = loader.load()

    text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = text_splitter.split_documents(data)

    vectordb_file_path = "./StoreFAISSCSV"

    if os.path.exists(vectordb_file_path):
        vectordb = FAISS.load_local(vectordb_file_path, embeddings=embeddings, allow_dangerous_deserialization=True)
        print("Loaded existing FAISS index from disk.")
    else:
        vectordb = FAISS.from_documents(documents=split_docs, embedding=embeddings)
        vectordb.save_local(folder_path=vectordb_file_path)
        print("Created and saved new FAISS index to disk.")

    retriever = vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})
    return retriever

# Load PDF data
def load_pdf_data(file_paths):
    all_docs = []
    for file_path in file_paths:
        loader = PyPDFLoader(file_path=file_path)
        documents = loader.load()
        all_docs.extend(documents)

    text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = text_splitter.split_documents(all_docs)
    
    vectordb_file_path = r"StoreFAISSPDF"
    if os.path.exists(vectordb_file_path):
        vectordb = FAISS.load_local(vectordb_file_path, embeddings=embeddings, allow_dangerous_deserialization=True)
        print("Loaded existing FAISS index from disk.")
    else:
        vectordb = FAISS.from_documents(documents=split_docs, embedding=embeddings)
        vectordb.save_local(folder_path=vectordb_file_path)
        print("Created and saved new FAISS index to disk.")

    return vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})

# Load CSV and PDF vector databases
csv_retriever = load_csv_data("./HowToDataStore.csv")
pdf_retriever = load_pdf_data([
    "./Reference/12.SD-Sales Monitoring and Analytics.pdf", 
    "./Reference/13.SD-Special Business Processes in Sales.pdf",
    "./Reference/14.SD-Integrations.pdf"
])

# Helper function to serialize Document objects
def serialize_document(doc):
    return {
        "page_content": str(doc.page_content),
        "metadata": {
            "source": str(doc.metadata.get("source", "")),
            "page": str(doc.metadata.get("page", 0))
        }
    }

# Flask API endpoints
@app.route('/csv/query', methods=['POST'])
def query_csv():
    data = request.json
    query = data.get("query", "")
    
    if not query:
        return jsonify({"error": "No query provided"}), 400

    rag_chain = create_csv_chain(csv_retriever)
    response = rag_chain.invoke({"query": query})

    serializable_sources = [serialize_document(doc) for doc in response.get("source_documents", [])]

    return jsonify({
        "answer": str(response["result"]),
        "sources": serializable_sources
    })

@app.route('/pdf/query', methods=['POST'])
def query_pdf():
    data = request.json
    query = data.get("query", "")
    
    if not query:
        return jsonify({"error": "No query provided"}), 400

    rag_chain = create_pdf_chain(pdf_retriever)
    response = rag_chain.invoke({"query": query})

    # Extract image and link information
    image_info, link_info = extract_pdf_info(response["source_documents"])
    image_data = [base64.b64encode(open(image_path, "rb").read()).decode("utf-8") for image_path in image_info]

    serializable_sources = [serialize_document(doc) for doc in response.get("source_documents", [])]

    return jsonify({
        "answer": response["result"],
        "sources": serializable_sources,
        "images": image_data,
        "links": link_info
    })

# Chains creation
def create_csv_chain(retriever):
    prompt_template = """
    You are an assistant for generating responses for CSV-based queries. Use the following context extracted from CSV data to answer the question:
    
    Context: {context}
    
    Question: {question}
    
    If the answer is not in the context, say "we dont have any information related to the search content, However below details will help for you."
    """
    
    PROMPT = PromptTemplate(
        template=prompt_template, 
        input_variables=["context", "question"]
    )
    
    chain = RetrievalQA.from_chain_type(
        llm=ChatOpenAI(model="gpt-4"),
        chain_type="stuff",
        retriever=retriever,
        return_source_documents=True,
        chain_type_kwargs={"prompt": PROMPT}
    )
    
    return chain
    
def create_pdf_chain(retriever):
    prompt_template = """You are a PDF Reader AI Assistant. Your task is to take in the user query and generate the answer only from the context provided. 
    
Context: {context}

Question: {question}"""
    
    PROMPT = PromptTemplate(
        template=prompt_template, 
        input_variables=["context", "question"]
    )
    
    chain = RetrievalQA.from_chain_type(
        llm=ChatOpenAI(model="gpt-4"),
        chain_type="stuff",
        retriever=retriever,
        return_source_documents=True,
        chain_type_kwargs={"prompt": PROMPT}
    )
    
    return chain

# Extract image and link information
def extract_pdf_info(source_documents):
    image_info = []
    link_info = []
    
    output_dir = r"ImagesExtracted"
    for source in source_documents:
        file_path = source.metadata['source']
        page_num = source.metadata.get('page', 0)
        
        # Extract images and links from PDF pages
        images = extract_images_from_pdf(file_path, page_num, output_dir)
        links = extract_links_from_pdf(file_path, page_num)

        image_info.extend(images)
        link_info.extend(links)

    return image_info, link_info

# Extract images from PDF
def extract_images_from_pdf(file_path, page_num, output_dir):
    document = fitz.open(file_path)
    page = document.load_page(page_num)
    image_list = page.get_images(full=True)
    
    images_info = []
    for img in image_list:
        xref = img[0]
        base_image = document.extract_image(xref)
        image_bytes = base_image["image"]
        image_ext = base_image["ext"]
        
        # Save the image locally and add its path to the images_info
        image_filename = f"{output_dir}/page_{page_num}_{xref}.{image_ext}"
        with open(image_filename, "wb") as f:
            f.write(image_bytes)
        
        images_info.append(image_filename)
    
    return images_info

# Extract links from PDF
def extract_links_from_pdf(file_path, page_num):
    document = fitz.open(file_path)
    page = document.load_page(page_num)
    links = page.get_links()
    
    links_info = []
    for link in links:
        if link['uri']:  # Checks if the link is a URL
            links_info.append(link['uri'])
    
    return links_info

# Run the Flask app
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8501, debug=True)