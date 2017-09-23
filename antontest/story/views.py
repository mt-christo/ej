from django.shortcuts import render
from django.http import HttpResponse
from django.shortcuts import render_to_response
from .models import Product, Provider
import numpy as np
import random

# Create your views here.


def allproducts(request):
    return render_to_response("story/allproducts.html", {'products' : Product.objects.all()})

def product(request,id):
    n = np.array(range(1000)) + 1
    d = np.cumsum(np.array([random.random() for x in n]) - 0.5)
    data = [[i,d[i]] for i in n]
    product = Product.objects.get(pk=id)
    return render_to_response("story/product.html", {'product' : product, 'data':data})

