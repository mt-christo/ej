from django.shortcuts import render
from django.http import HttpResponse
from django.shortcuts import render_to_response
from .models import Product, Provider

# Create your views here.


def home(request):
    return render_to_response("story/home.html", {'products' : Product.objects.all()})

def home2(request):
    return HttpResponse('Hello AAAABBBB!!!!!!!!!!')

