from dotenv import load_dotenv
from langchain_community.document_loaders import PyPDFDirectoryLoader, CSVLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_openai import OpenAIEmbeddings
from langchain_community.vectorstores.faiss import FAISS
import os

load_dotenv()

def load_documents(loader, splitter=None):
    docs = loader.load()
    return splitter.split_documents(docs) if splitter else docs

def create_faiss_db(docs, db_path):
    embedding = OpenAIEmbeddings()

    vectorStore = FAISS.from_documents(docs, embedding=embedding)
    vectorStore.save_local(db_path)

if __name__ == '__main__':
    pdf_docs = load_documents(PyPDFDirectoryLoader("./pdfs"), RecursiveCharacterTextSplitter(
        separators=['\n\n', '\n', '.', ','], chunk_size=1000, chunk_overlap=200))
    create_faiss_db(pdf_docs, "faiss_pdf_index")
    print("PDFs loaded into FAISS and saved to disk")

    csv_docs = load_documents(CSVLoader(file_path="./csvs/HowToDataStore.csv", source_column="prompt", encoding="iso-8859-1"))
    create_faiss_db(csv_docs, "faiss_csv_index")
    print("CSVs loaded into FAISS and saved to disk")
