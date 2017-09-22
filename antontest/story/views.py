from django.shortcuts import render
from django.http import HttpResponse
from django.shortcuts import render_to_response
from .models import Product, Provider

# Create your views here.


def allproducts(request):
    return render_to_response("story/allproducts.html", {'products' : Product.objects.all()})

def product(request,id):
    return render_to_response("story/product.html", {'product' : Product.objects.get(pk=id)})

