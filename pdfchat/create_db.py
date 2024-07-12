from dotenv import load_dotenv
from langchain_community.document_loaders import PyPDFDirectoryLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_openai import OpenAIEmbeddings
from langchain_qdrant import Qdrant
import os

load_dotenv()

url = os.environ["QDRANT_URL"]
collection_name = os.environ["QDRANT_COLLECTION"]

instructor_embeddings = OpenAIEmbeddings()

def get_documents(path):
    loader = PyPDFDirectoryLoader(path)
    docs = loader.load()
    splitter = RecursiveCharacterTextSplitter(
        separators=['\n\n', '\n', '.', ','],
        chunk_size=1000,
        chunk_overlap=200
    )
    splitDocs = splitter.split_documents(docs)
    return splitDocs

def create_db(docs):
    embeddings = instructor_embeddings
    Qdrant.from_documents(
        docs,
        embeddings,
        url=url,
        prefer_grpc=True,
        collection_name=collection_name,
        force_recreate=True,
    )

if __name__ == '__main__':
    docs = get_documents("./pdfs")
    create_db(docs)