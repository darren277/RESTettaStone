import json

from django.http import JsonResponse
from django.views.decorators.csrf import csrf_exempt

from .models import User

@csrf_exempt
def users(request):
    if request.method == 'POST':
        return post_user(request)
    users = User.objects.all()
    context = [user.toJSON() for user in users]
    return JsonResponse(context, safe=False)

@csrf_exempt
def user(request, id):
    try:
        user = User.objects.get(id=id)
    except User.DoesNotExist:
        return JsonResponse({"error": "User not found"}, status=404)
    if request.method == 'GET':
        context = user.toJSON()
        return JsonResponse(context, safe=False)
    elif request.method == 'PUT':
        return update_user(user, request, id)
    elif request.method == 'DELETE':
        return delete_user(user, id)
    else:
        return JsonResponse({"error": "Method not allowed"}, status=405)

def post_user(request):
    try:
        body = json.loads(request.body)
        email = body.get("email")
        user = User.objects.create(email=email)
        context = user.toJSON()
        return JsonResponse(context, status=200)
    except (ValueError, KeyError) as e:
        return JsonResponse({"error": str(e)}, status=400)

def update_user(user, request, id):
    try:
        body = json.loads(request.body)
        email = body.get("email")
    except (ValueError, KeyError) as e:
        return JsonResponse({"error": str(e)}, status=400)
    user.email = email
    user.save()
    context = user.toJSON()
    return JsonResponse(context, safe=False)

def delete_user(user, id):
    user.delete()
    return JsonResponse({'status': 'success', 'id': id}, safe=False)

