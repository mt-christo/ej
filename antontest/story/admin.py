from django.contrib import admin
from .models import Product, Provider

@admin.register(Product)
class pa(admin.ModelAdmin):  
  list_display = ('name', 'titlethumb', 'titlecard', 'logopath', 'explainpath', 'explainpath2', 'tspath', 'templatepath', 'participation', 'coupon', 'listProviders')

@admin.register(Provider)
class pr(admin.ModelAdmin):  
  list_display = ('name', 'longname', 'url', 'imgpath', 'desc')
  
# Register your models here.
