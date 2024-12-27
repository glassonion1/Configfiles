# docker exec
alias dockerexec='docker exec -it $(docker ps | peco | cut -d " " -f 1) /bin/bash'
