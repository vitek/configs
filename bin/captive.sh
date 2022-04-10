xdg-open http://$(ip --oneline route get 1.1.1.1 | awk '{print $3}')
