from dotenv import load_dotenv
from langchain_community.document_loaders import PyPDFDirectoryLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_openai import OpenAIEmbeddings
from langchain_google_genai import GoogleGenerativeAIEmbeddings
from langchain_qdrant import Qdrant
import os

load_dotenv()

url = os.environ["QDRANT_URL"]

instructor_embeddings = OpenAIEmbeddings()

def get_pdfs(path):
    loader = PyPDFDirectoryLoader(path)
    docs = loader.load()
    splitter = RecursiveCharacterTextSplitter(
        separators=['\n\n', '\n', '.', ','],
        chunk_size=1000,
        chunk_overlap=200
    )
    splitDocs = splitter.split_documents(docs)
    return splitDocs

def create_db_pdf(docs):
    Qdrant.from_documents(
        docs,
        embedding = OpenAIEmbeddings(),
        url=url,
        prefer_grpc=True,
        collection_name=os.environ["QDRANT_PDF_OPENAI_COLLECTION"],
        force_recreate=True,
    )
    Qdrant.from_documents(
        docs,
        embedding = GoogleGenerativeAIEmbeddings(model="models/embedding-001"),
        url=url,
        prefer_grpc=True,
        collection_name=os.environ["QDRANT_PDF_GEMINI_COLLECTION"],
        force_recreate=True,
    )
    
def create_db_csv(docs):
    Qdrant.from_documents(
        docs,
        embedding = OpenAIEmbeddings(),
        url=url,
        prefer_grpc=True,
        collection_name=os.environ["QDRANT_CSV_OPENAI_COLLECTION"],
        force_recreate=True,
    )
    Qdrant.from_documents(
        docs,
        embedding = GoogleGenerativeAIEmbeddings(model="models/embedding-001"),
        url=url,
        prefer_grpc=True,
        collection_name=os.environ["QDRANT_CSV_GEMINI_COLLECTION"],
        force_recreate=True,
    )

if __name__ == '__main__':
    docs = get_pdfs("./pdfs")
    create_db_pdf(docs)
    print("PDFs loaded")
    # docs = get_documents("./csvs")
    # create_db_csv(docs)
    # print("CSVs loaded")