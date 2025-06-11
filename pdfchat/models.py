# models.py
from mongoengine import Document, IntField, StringField, FloatField

# IDoc Collection Schema
class IDoc(Document):
    meta = {'collection': 'idoc'}
    
    idoc_number = IntField(required=True, unique=True)
    date = StringField()
    time = StringField()
    status = IntField()
    status_code = IntField()
    status_text = StringField()
    routine_function_mode = StringField()
    order_number = IntField()
    sales_org = IntField()
    sales_area = IntField()
    division = IntField()
    amount = FloatField()


# MasterData Collection Schema
class MasterData(Document):
    meta = {'collection': 'master_data'}
    
    routine_function_code = StringField(required=True, unique=True)
    order_number = IntField(required=True, unique=True)
    sales_org = IntField()
    sales_area = IntField()
    division = IntField()
    amount = FloatField()
