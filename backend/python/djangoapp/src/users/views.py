from django.http import JsonResponse

from .models import User

def users(request):
    if request.method == 'POST':
        return post_user(request)
    #a_list = User.objects.filter(pub_date__year=year)
    users = User.objects.all()
    context = [user.toJSON() for user in users]
    return JsonResponse(context, safe=False)

def get_user(request, id):
    user = User.objects.get(id=id)
    context = user.toJSON()
    return JsonResponse(context, safe=False)

def user(request, id):
    if request.method == 'GET':
        return get_user(request, id)
    elif request.method == 'PUT':
        return update_user(request, id)
    elif request.method == 'DELETE':
        return delete_user(request, id)
    user = User.objects.get(id=id)
    context = user.toJSON()
    return JsonResponse(context, safe=False)

def post_user(request):
    name = request.POST.get('name')
    email = request.POST.get('email')
    user = User(name=name, email=email)
    user.save()
    context = user.toJSON()
    return JsonResponse(context, safe=False)

def update_user(request, id):
    user = User.objects.get(id=id)
    user.name = request.POST.get('name')
    user.email = request.POST.get('email')
    user.save()
    context = user.toJSON()
    return JsonResponse(context, safe=False)

def delete_user(request, id):
    user = User.objects.get(id=id)
    user.delete()
    return JsonResponse({'status': 'success'}, safe=False)

