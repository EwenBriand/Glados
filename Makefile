##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile of Glad0s
##

NAME    = glados

ROUTE	:=  $(shell stack path --local-install-root)

all: $(NAME)

$(NAME):
	stack setup && stack build
	cp $(ROUTE)/bin/$(NAME)-exe .
	mv $(NAME)-exe $(NAME)

clean:
	rm $(NAME) -f

fclean: clean
	stack clean

re:	fclean all
