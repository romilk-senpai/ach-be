docker-compose down db
docker volume rm ach_pgdata
docker-compose up -d db
