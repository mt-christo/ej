from django.shortcuts import render
from django.http import HttpResponse
from django.shortcuts import render_to_response
from .models import Product, Provider
import numpy as np
import pandas as pd
import random

# Create your views here.


def allproducts(request):
    #return render_to_response("story/allproducts.html", {'products' : Product.objects.all()})
    return render_to_response("story/tmp.html", {'products' : Product.objects.all()})

def product(request,id):
    product = Product.objects.get(pk=id)
    #x.apply(lambda row: "[new Date('" + str(row.Date) + "'), " + str(row.Close),axis=1) + "]"
    x = pd.read_csv('story/static/' + product.tspath)
    data = np.array(x.apply(lambda row: [str(row.Date), row.Close],axis=1))
    return render_to_response("story/" + product.templatepath, {'product': product, 'data': data})

