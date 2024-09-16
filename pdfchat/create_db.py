import os
from langchain_openai import OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader, CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Initialize OpenAI embeddings
embeddings = OpenAIEmbeddings(model="text-embedding-3-large")

def create_vector_store(file_paths, loader_class, vectordb_path):
    """
    Create a vector store for given files using the specified loader.
    """
    all_docs = []
    for file_path in file_paths:
        loader = loader_class(file_path=file_path)
        documents = loader.load()
        all_docs.extend(documents)

    text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = text_splitter.split_documents(all_docs)
    
    vectordb = FAISS.from_documents(documents=split_docs, embedding=embeddings)
    vectordb.save_local(folder_path=vectordb_path)
    print(f"Created and saved new FAISS index to {vectordb_path}")

if __name__ == "__main__":
    # Create CSV vector store
    create_vector_store(["./output.csv"], CSVLoader, "./StoreFAISSCSV")

    # Create PDF vector store
    pdf_files = [
        "./Reference/12.SD-Sales Monitoring and Analytics.pdf", 
        "./Reference/13.SD-Special Business Processes in Sales.pdf",
        "./Reference/14.SD-Integrations.pdf",
        "./Reference/SAP IM Database.pdf"
    ]
    create_vector_store(pdf_files, PyPDFLoader, "./StoreFAISSPDF")