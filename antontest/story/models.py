from django.db import models

# Create your models here.

class Product(models.Model):
    name = models.CharField(max_length=255, default='[NO NAME]')
    descshort = models.CharField(max_length=2000, default='')
    logopath = models.CharField(max_length=255, default='')
    explainpath = models.CharField(max_length=255, default='')
    tspath = models.CharField(max_length=255, default='')
    templatepath = models.CharField(max_length=255, default='')
    participation = models.CharField(max_length=20, default='')
    coupon = models.CharField(max_length=20, default='')
    provider = models.ManyToManyField('Provider')
    
    def listProviders(self):
        return(", ".join([str(p.name) for p in self.provider.all()]))
    
    def allProviders(self):
        return(self.provider.all())

class Provider(models.Model):
    name = models.CharField(max_length=255, default='')
    longname = models.CharField(max_length=500, default='')
    url = models.CharField(max_length=500, default='')
    imgpath = models.CharField(max_length=255, default='')
    desc = models.CharField(max_length=5000, default='')
    
    def __str__(self):
        return(self.name)
    
