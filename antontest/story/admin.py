from django.contrib import admin
from .models import Product, Provider

@admin.register(Product)
class pa(admin.ModelAdmin):  
  list_display = ('name', 'imgpath', 'descshort', 'part', 'coupon', 'desclong', 'provider.name')

@admin.register(Provider)
class pr(admin.ModelAdmin):  
  list_display = ('name', 'imgpath', 'desc')
  
# Register your models here.
