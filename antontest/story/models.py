from django.db import models

# Create your models here.

class Product(models.Model):
    name = models.CharField(max_length=255, default='[NO NAME]')
    imgpath = models.CharField(max_length=255, default='')
    descshort = models.CharField(max_length=2000, default='')
    part = models.CharField(max_length=2000, default='')
    coupon = models.CharField(max_length=2000, default='')
    desclong = models.CharField(max_length=2000, default='')
    provider = models.ManyToManyField('Provider')
    
    def providerName(self):
        return(provider.name)

class Provider(models.Model):
    name = models.CharField(max_length=255, default='')
    imgpath = models.CharField(max_length=255, default='')
    desc = models.CharField(max_length=2000, default='')
    
