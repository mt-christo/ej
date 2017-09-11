from django.db import models

# Create your models here.

class Product(models.Model):
    name = models.CharField(max_length=255)
    imgpath = models.CharField(max_length=255)
    descshort = models.CharField(max_length=2000)
    part = models.CharField(max_length=2000)
    coupon = models.CharField(max_length=2000)
    desclong = models.CharField(max_length=2000)
    provider = models.ManyToManyField('Provider')

class Provider(models.Model):
    name = models.CharField(max_length=255)
    imgpath = models.CharField(max_length=255)
    desc = models.CharField(max_length=2000)
    
