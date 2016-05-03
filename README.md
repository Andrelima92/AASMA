# AASMA
Proj AASMA Instructions



reactive, Deliberative and learning social agents

Ambiente:
 
4 predadores com cores diferentes
1 presa

toroidal grid-world size n * n
pass o limite aparece do outro lado.

objetivo: capturar a presa cercando-a
presa e os predadores nao podem estar no mesmo tile.
posições iniciais random.


Predadores têm visão limitada d

2d+1<n
tamanho da matriz (2d+1)^2
ações simultâneas.
Se 2 decidem ir para a mesma cell dá fail e eles ficam no mesmo sitio.

Agentes homogéneos - mesmo objetivo, mesmas capacidades, bem comum (valor de capturar uma presa é partilhado)
Cada agente individual comporta-se e aprende consoante as suas percepções a cada tempo e ignora as ações dos outros agentes.

Presa: Pode ser inteligente mas podemos deixar random - a cada t move-se para uma posição adjacente ou fica quieta com uma determinada probabilidade.
Episódios - presa capturada, time limit.


Arquiteturas de agentes:


Agentes Reativos:
	Várias abordagens no [3]

	Devem ser garantidas restrições realistas - não há comunicação nem memória.
	Problemas que surjam em relação a comportamentos conjuntos devem ser tratados de forma reactiva.

Agentes Deliberativos:
	Podem extender comportamentos reativos para maior complexidade: procurar o melhor caminho, dar alvo a várias posições, usar formações através de forças,etc.
 BDI e desires são opcionais.
Se usarmos comunicação temos de usar custos realistas.

	Agentes com Aprendizagem:
	Reinforcement Learning. 
	Usar game theory para atingir equilibrios.
	agentes podem aprender através das atividades dos outros (irrealista?)
	individualmente é difícil a coordenação se não puderem comunicar ou partilhar informação-
	Visto que vários agentes têm de fazer a tarefa em conjunto o espaço de aprendizagem para cada agente cresce exponencialmente.
	RL modular approach proposta em [2]. Decompor os problemas de dimensão alta em problemas com vários Q-learning módulos em paralelo.(para cada agente).
	Cada agente deve aprender individual, nao se pode por um a controlar os outros,
	Pode-se usar algoritmos da literatura.
	Mais informação de técnicas single agent RL no [4]


	Abordagens Híbridas:
	Equipas mistas de vários tipos de agentes. E comparar as performances do grupo. 
	Pode-se considerar diferentes percepções também para caso de estudo.

	Objetivos:
	Modelar Cenário:
	Ambiente Pursuit - area retangular de tamanho variável.
	Dois tipos de entidades (agente, presa)
	Sensores e aturadores dos agentes.
	Dinâmica do sistema - o próximo estado resultante das ações.

	Parâmetros
	Tamanho e forma do mundo
	Movimentos legais dos predadores.
	Movimento dos predadores 
	Campo de visão
	Movimento da presa,

	Métricas de Performance:
	Quantidade de presas capturadas.
	Tempo para capturar cada presa
	Número de mensagens trocadas,
	Número de colisões nas decisões de predadores.


	Arquiteturas:
	Relativa sem estado interno
	Deliberativa com alguma forma de raciocínio,
	Os agentes são individual learners


Referências:

[1]  M. Benda, V. Jagannathan, and R. Dodhiawala. “On optimal cooperation of knowledge sources

- an empirical investigation”. Technical Report BCS–G2010–28, Boeing Advanced Technology Center, Boeing Computing Services, Seattle, Washington, July 1986.

[2]  N.OnoandK.Fukumoto.“Mulit-agentreinforcementlearning:Amodularapproach”.InSecond International Conference on Multiagent Systems, pp. 252-258, Kyoto, 1996.

[3]  P. Stone and M. Veloso. “Multiagent systems: A survey from a machine learning perspective”. Autonomous Robots, 8(3), 2000.

[4]  Sutton, R. S., and Barto, A. G. “Reinforcement Learning”, MIT Press, Cambridge, MA, 1998. Online at: https://webdocs.cs.ualberta.ca/~sutton/book/the-book.html

 

	
