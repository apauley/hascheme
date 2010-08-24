default:
	./Setup.lhs configure --prefix=${HOME} --user
	./Setup.lhs build -v
	./Setup.lhs install
