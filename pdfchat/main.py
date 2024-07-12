from flask import Flask, request, jsonify
from flask_cors import CORS
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain.chains.combine_documents import create_stuff_documents_chain
from langchain.chains import create_retrieval_chain
from langchain_core.messages import HumanMessage, AIMessage
from langchain.chains.history_aware_retriever import create_history_aware_retriever
from langchain_qdrant import Qdrant
import os

app = Flask(__name__)
CORS(app, origins="*")

load_dotenv()

url = os.environ["QDRANT_URL"]
collection_name = os.environ["QDRANT_COLLECTION"]

instructor_embeddings = OpenAIEmbeddings()

def create_chain():
    vectorStore = Qdrant.from_existing_collection(
                    embedding=instructor_embeddings,
                    collection_name=collection_name,
                    url=url,
                )
    model = ChatOpenAI(
        model="gpt-3.5-turbo",
        temperature=0.5
    )

    prompt = ChatPromptTemplate.from_messages([
        ("system", "Given the following context and a question, generate an answer based on this context as much as possible. Give detailed answers. at least 100 words. In markdown format with paragraphs, headings and bullet points. In the answer, try to provide as much text as possible from the \"response\" section in the source document context without making many changes. If the information is not available in the context, you may then search the internet to provide an answer. Don't ask for confirmation. Just give the answer.: {context}"),
        MessagesPlaceholder(variable_name="chat_history"),
        ("human", "{input}")
    ])

    chain = create_stuff_documents_chain(
        llm=model,
        prompt=prompt,
    )

    retriever = vectorStore.as_retriever(score_threshold=0.7)

    retriever_prompt = ChatPromptTemplate.from_messages([
        MessagesPlaceholder(variable_name="chat_history"),
        ("human", "{input}"),
        ("human", "Given the above conversation, generate a search query to look up in order to get information relevant to the conversation")
    ])

    history_aware_retriever = create_history_aware_retriever(
        llm=model,
        retriever=retriever,
        prompt=retriever_prompt
    )

    retrieval_chain = create_retrieval_chain(
        history_aware_retriever,
        chain,
    )

    return retrieval_chain

def process_chat(chain, question, chat_history):
    response = chain.invoke({
        "input": question,
        "chat_history": chat_history
    })
    return response

@app.route('/chat', methods=['POST'])
def chat():
    data = request.get_json()
    question = data['prompt']
    response = process_chat(chain, question, chat_history)
    chat_history.append(HumanMessage(content=question))
    chat_history.append(AIMessage(content=response["answer"]))
    
    sources = [{"page_content": doc.page_content, "metadata": doc.metadata} for doc in response["context"]]
    return jsonify({"answer": response["answer"], "sources": sources})

if __name__ == '__main__':
    chain = create_chain()
    chat_history = []
    app.run(host='0.0.0.0', port=8501, debug=True)