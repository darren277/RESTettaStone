from django.http import JsonResponse

from .models import User

#def get_users(request, email):
def get_users(request):
    #a_list = User.objects.filter(pub_date__year=year)
    users = User.objects.all()
    context = [user.toJSON() for user in users]
    return JsonResponse(context, safe=False)
