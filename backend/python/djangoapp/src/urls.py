from django.conf.urls.i18n import i18n_patterns
from django.contrib import admin
from django.urls import include, path
from django.views.generic import TemplateView


urlpatterns = i18n_patterns(
    #path('', TemplateView.as_view(template_name="index.html")),
    path('admin/', admin.site.urls),
    path('djangoapp/', include('djangoapp.users.urls'))
)
