##
## EPITECH PROJECT, 2025
## glados-vm
## File description:
## Makefile
##


BINARY_PATH :=	$(shell stack path --local-install-root)

NAME = glados-vm

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	rm -rf .stack-work
	rm -rf ./test/coverage

re: fclean all

unit_tests:
	stack test --coverage
	stack hpc report glados-vm
	mkdir -p ./test/coverage
	cp -r $(BINARY_PATH) ./test/coverage/

tests_run: unit_tests

.PHONY: all clean fclean re
