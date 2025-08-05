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

# PO Automation Collection Schema
class POAutomation(Document):
    meta = {'collection': 'po_automation'}
    pr_number = IntField(required=True)
    material = StringField()
    product_group = StringField()
    quantity = FloatField()
    purchase_order_Qty = FloatField()
    total_value = FloatField()
    assigned_supplier = StringField()
    delivery_date = StringField()
    plant = IntField()
    status = StringField()
    comments = StringField()
    po_number = IntField()
class Apsuite(Document):
    meta = {'collection': 'apsuite'}
    entityType = StringField(required=True)
    typeOfData = StringField(required=True)
    apsuiteName = StringField(required=True)
    sapTableName = StringField(required=True)
    sapFieldName = StringField(required=True)
    apiName = StringField(required=True)
    endpoint = StringField(required=True)