from django.urls import path

from . import views

urlpatterns = [
    path("users/", views.get_users),
    #path("users/<int:year>/", views.year_archive),
]
