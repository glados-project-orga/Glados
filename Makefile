##
## EPITECH PROJECT, 2025
## glados
## File description:
## Makefile
##


BINARY_PATH :=	$(shell stack path --local-install-root)

NAME = glados

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	rm -rf .stack-work

re: fclean all

.PHONY: all clean fclean re