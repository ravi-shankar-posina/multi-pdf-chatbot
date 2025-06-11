# dbconn.py
import os
from dotenv import load_dotenv
from mongoengine import connect

load_dotenv()

MONGODB_URI = os.getenv("MONGODB_URI")

connect(host=MONGODB_URI, alias="default")