:-
	write('=============================================='), nl,
	write('Cadastro e movimentação de contas bancárias'), nl,
	write('Para executar a primeira vez, digite na consulta:'), nl,
	write('?- inicio.'), nl,
	write('Para executar a segunda vez em diante, digite na consulta:'), nl,
	write('?- banco.'),nl,
	write('=============================================='), nl.

inicio :-
	leArq,
	banco, !.

leArq :- 
	write('Informe o nome do arquivo de dados para leitura: '),
	read(A),
	exists_file(A), 
	(
		see(A), 
   		leitura, !
	)
	| 
	write('Arquivo nao existe'), nl, 
	write('O nome do arquivo deve ser digitado entre apóstrofos'),
	nl, leArq. 

leitura :-
	read(Dados),
	Dados \= end_of_file, 
	( 
		assert(Dados), 
		leitura
	)
	| seen, !.

banco :-
	write('===================================='), nl,
	write('[1] Cadastrar conta'), nl,
	write('[2] Depositar'), nl,
	write('[3] Sacar'), nl,
	write('[4] Transferir'), nl,
	write('[5] Saldo'), nl,
	write('[6] Extrato'), nl,
	write('[7] Análise'), nl,
	write('[8] Sair e gravar'), nl,
	write('[0] Sair sem gravar'), nl,
	write('Informe a operacao: '),
	read(Op),
	write('===================================='), nl,
	executa(Op), !.

executa(1) :- 
	write('Cadastro de conta'), nl,
	write('Informar codigo da conta: '),
	read(Codigo),
	write('Informe saldo inicial: '),
	read(SaldoInicial),
	cadastrarConta(Codigo,SaldoInicial),
	banco.

executa(2) :- 
	write('Deposito'), nl,
	write('Informar codigo da conta: '),
	read(Codigo),
	write('Informe valor a depositar: '),
	read(Deposito),
	depositar(Codigo,Deposito),
	banco.

executa(3) :- 
	write('Saque'), nl,
	write('Informar codigo da conta: '),
	read(Codigo),
	write('Informe valor a sacar: '),
	read(Saque),
	sacar(Codigo,Saque),
	banco.

executa(4) :- 
	write('Transferencia'), nl,
	write('Informe codigo da conta origem: '),
	read(CodigoOrigem),
	write('Informe codigo da conta destino: '),
	read(CodigoDestino),
	write('Informe o valor a transferir: '),
	read(Valor),
	transferir(CodigoOrigem,Valor,CodigoDestino),
	banco.

executa(5) :- 
	write('Verificacao de saldo'), nl,
	write('Informar codigo da conta: '),
	read(Codigo),
	verSaldo(Codigo),
	banco.

executa(6) :- 
	write('Apresentacao de extrato'), nl,
	write('Informar codigo da conta: '),
	read(Codigo),
	extrato(Codigo),
	banco.

executa(7) :-
	analisa.

executa(8) :-
	write('Sair e gravar'), nl,
	write('Informe o nome do arquivo de dados para gravacao: '),
	read(A),
	tell(A), 
	forall(c(C,S), 
		(
			write(c(C,S)), put('.'),
			nl
		)
	),
	forall(d(C,D), 
		(
			write(d(C,D)), write('.'),
			nl
		)
	),
	forall(s(C,S), 
		(
			write(s(C,S)), write('.'),
			nl
		)
	),
	forall(t(C1,V,C2), 
		(
			write(t(C1,V,C2)), write('.'),
			nl
		)
	),
	told,
	write('Fim'), nl, !.

executa(0) :-
	write(	'Sair sem gravar'), nl,
	write('Fim'), nl, !.

executa(_) :- % Operacao inexistente
	write('Operacao inexistente'), 
	nl.

cadastrarConta(Codigo,SaldoInicial) :-
	findall(C,c(C,_),L),
	member(Codigo,L), 
	(
		write('Conta com codigo '),
		write(Codigo),
		write(' ja existe'),
		nl, !
	)
	| 
	assert(c(Codigo,SaldoInicial)),
	verSaldo(Codigo).

depositar(Codigo,Deposito) :-
	Deposito > 0,
	c(Codigo,Saldo),
	(
		retract(c(Codigo,Saldo)),
		SaldoAtual is Saldo + Deposito,
		assert(c(Codigo,SaldoAtual)),
		assert(d(Codigo,Deposito)),
		write('Deposito efetuado'), nl,
		verSaldo(Codigo), !
	)
	| write('Conta com este codigo nao existe'), nl.

sacar(Codigo,Saque) :-
	Saque > 0,
	c(Codigo,Saldo),
	(
		Saldo >= Saque,
		(
			retract(c(Codigo,Saldo)),
			SaldoAtual is Saldo - Saque,
			assert(c(Codigo,SaldoAtual)),
			assert(s(Codigo,Saque)),
			write('Saque efetuado'), nl,
			verSaldo(Codigo), !
		)
		| write('Saldo insuficiente'), nl, !
	)
	| write('Conta com este codigo nao existe'), nl.

verSaldo(Codigo) :-
	forall(c(Codigo,Saldo), 
		(
			write('Conta '), 
			write(Codigo), 
			write(' tem saldo de R$'), 
			write(Saldo), nl
		)
	).

extrato(Codigo) :-
	verSaldo(Codigo),
	forall(d(Codigo,D),
		(
			write('Deposito de '), 
			write(D), nl
		)
	),
	forall(s(Codigo,S),
		(
			write('Saque de '), 
			write(S), nl
		)
	),
	forall(t(Codigo,C,V),
		(
			write('Transferencia de '), 
			write(V),
			write(' para a conta '),
			write(C), nl
		)
	).

transferir(Codigo1,Valor,Codigo2) :-
	c(Codigo2,_), % evita sacar de Codigo1 se Codigo2 nao existe
	(
		sacar(Codigo1,Valor),
		depositar(Codigo2,Valor), 
		assert(t(Codigo1,Codigo2,Valor)), !
	)
	| 
	(
		write('Conta com codigo '), 
	 	write(Codigo2),
		write(' nao existe'), nl
	).

analisa :-
	write('===================================='), nl,
	write('Analise'), nl,
	write('[1] Média dos saldos de todas as contas do banco'), nl,
	write('[2] Códigos e saldos das contas com saldo maior que um dado valor'), nl,
	write('[3] Diferença (total dos depósitos menos total dos saques) no banco'), nl,
	write('[4] Total dos saldos de todas as contas do banco'), nl,
	write('[5] Voltar'), nl,
	write('[0] Sair sem gravar'), nl,
	write('Informe a operacao: '),
	read(Op),
	write('===================================='), nl,
	analisa(Op), !.

analisa(1) :- 
	write('Falta fazer a opção 1 da análise'),
	nl,
	analisa.

analisa(2) :- 
	write('Falta fazer a opção 2 da análise'),
	nl,
	analisa.

analisa(3) :- 
	write('Falta fazer a opção 3 da análise'),
	nl,
	analisa.

analisa(4) :- 
	write('Falta fazer a opção 4 da análise'),
	nl,
	analisa.

analisa(5) :- % Voltar
	banco.

analisa(0) :- % Sair sem gravar
	executa(0).

soma([],0).
soma([C|R],Total) :-
	soma(R,TR),
	Total is C + TR.

mostraContas([]).
mostraContas([(Codigo,Saldo)|R]) :-
	write('Codigo '),
	write(Codigo),
	write(': R$'),
	write(Saldo),
	nl,
	mostraContas(R).

