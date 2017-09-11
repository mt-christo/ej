from django.contrib import admin
from .models import Product, Provider

@admin.register(Product)
class pa(admin.Modeladmin):  
  list_display = ('name', 'imgpath', 'descshort', 'part', 'coupon', 'desclong', 'provider')

@admin.register(Provider)
class pr(admin.Modeladmin):  
  list_display = ('name', 'imgpath', 'desc')
  
# Register your models here.
