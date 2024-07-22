from dotenv import load_dotenv
from langchain_community.document_loaders import PyPDFDirectoryLoader, CSVLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_openai import OpenAIEmbeddings
from langchain_google_genai import GoogleGenerativeAIEmbeddings
from langchain_qdrant import Qdrant
import os

load_dotenv()

url = os.getenv("QDRANT_URL")
pdf_openai_collection = os.getenv("QDRANT_PDF_OPENAI_COLLECTION")
pdf_gemini_collection = os.getenv("QDRANT_PDF_GEMINI_COLLECTION")
csv_openai_collection = os.getenv("QDRANT_CSV_OPENAI_COLLECTION")
csv_gemini_collection = os.getenv("QDRANT_CSV_GEMINI_COLLECTION")

def get_documents_from_pdfs(path):
    loader = PyPDFDirectoryLoader(path)
    docs = loader.load()
    splitter = RecursiveCharacterTextSplitter(
        separators=['\n\n', '\n', '.', ','],
        chunk_size=1000,
        chunk_overlap=200
    )
    split_docs = splitter.split_documents(docs)
    return split_docs

def get_documents_from_csv(path, source_column="prompt", encoding="iso-8859-1"):
    loader = CSVLoader(file_path=path, source_column=source_column, encoding=encoding)
    docs = loader.load()
    return docs

def create_qdrant_db(docs, collections):
    for collection_name, embedding in collections:
        Qdrant.from_documents(
            docs,
            embedding=embedding,
            url=url,
            prefer_grpc=True,
            collection_name=collection_name,
            force_recreate=True
        )

if __name__ == '__main__':
    # PDF processing
    pdf_docs = get_documents_from_pdfs("./pdfs")
    pdf_collections = [
        (pdf_openai_collection, OpenAIEmbeddings()),
        (pdf_gemini_collection, GoogleGenerativeAIEmbeddings(model="models/embedding-001"))
    ]
    create_qdrant_db(pdf_docs, pdf_collections)
    print("PDFs loaded")

    # CSV processing
    csv_docs = get_documents_from_csv("./csvs/HowToDataStore.csv")
    csv_collections = [
        (csv_openai_collection, OpenAIEmbeddings()),
        (csv_gemini_collection, GoogleGenerativeAIEmbeddings(model="models/embedding-001"))
    ]
    create_qdrant_db(csv_docs, csv_collections)
    print("CSVs loaded")