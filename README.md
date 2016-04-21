# hawk_client
<p align="center">
<img src="http://res.cloudinary.com/post-hawk/image/upload/v1461237300/%D0%BF%D1%80%D0%BE%D1%86%D0%B5%D1%81%D1%81_%D0%BE%D0%B1%D0%BC%D0%B5%D0%BD%D0%B0_nmytju.png"/>
</p>
Post Hawk - это сервис, предоставляющий простое и понятное API, для организации связи между мобильными и web приложениями в режиме реального времени.
Данный репозиторий содержит клиент для работы с сообщениями. [Сервер](https://github.com/postHawk/hawk_server)

Установка: 
```bash
git clone https://github.com/postHawk/hawk_client.git
cd hawk_client
nano src/hawk_client.app.src
#заполните название server_node, например, 'test_hawk_server@127.0.0.1' и сохраните файл
rebar get-deps compile
erl -name 'hawk_client@127.0.0.1' -boot start_sasl  -setcookie test -kernel inet_dist_listen_min 9000  inet_dist_listen_max 9005
```

Инструкцию по настройке клиентских библиотек можно найти в [wiki](https://github.com/postHawk/hawk_api/wiki)
