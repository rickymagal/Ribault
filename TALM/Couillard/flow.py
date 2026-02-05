#Dataflow graph generation
#Tiago A.O.A. and Leandro A. J. Marzulo
import cparse
from cvisitors import Visitor

from graphvizitor import GraphVizVisitor
from talmvisitor import TalmVisitor
from talmvisitor import DataFlowTrimVisitor
import pdb
import dbfuncs

import os
import sys
#from copy import deepcopy

#works only for dicts
def graph_build_error(error):
	print(error)
	
def semideepcopy(dictio):
	out = {}
	for key in dictio:
		out[key]=[x for x in dictio[key]]
	return out

def start_debug(n):
	if "DEBUG" in os.environ and os.environ["DEBUG"] == n:
		pdb.set_trace()	
class DataNode(cparse.Node):

	def __init__(self):
		self.input = []
		self.output = []
	def __repr__(self):
		#return str(self.__class__).replace('.', '') + "_" + str(id(self))
		return str(self.__class__).replace('.', '') + str(id(self))
	def isparallel(self):
		#print "Is inst parallel? %s" %self
		return False #instructions are single (not parallel) by default
class InstBinop(DataNode):
	def __init__(self, op, typestr):
		DataNode.__init__(self)
		self.op = op
		self.typestr = typestr
		self.trace_steers = [] #The steer ports corresponding to the branches taken
	#def __repr__(self):
	#	print "%s" %(self.op)

class InstBinopI(InstBinop):
	def __init__(self, op, typestr):
		InstBinop.__init__(self, op, typestr)


class InstConst(DataNode):
	def __init__(self, value, type):
	#	print "Const criada %s" %self
		self.value = value
		self.type = type
class InstSteer(DataNode):
	def __init__(self, expr, in_then=None):
		#self.input = input 
		self.t = None
		self.f = None
		self.created_signals = []
		self.expr = expr
		if in_then != None:
			self.in_then_stmt = in_then
		
	def clone(self):
		return InstSteer(self.expr, self.in_then_stmt)

	def create_true(self):
		self.t = SteerTrue(self)
		return self.t
	def create_false(self):
		self.f = SteerFalse(self)
		return self.f

	def isparallel(self):
		#print "Is inst steer parallel %s" %self#%(self, len([p for p in self.input if p.isparallel()]) > 0)	
		output= len([p for p in self.input if p.isparallel()]) > 0	
		#print "inst steer %s %s" %(self, output)
		return output

class InstIncTag(DataNode):
	def __init__(self, input):
		#self.name = name
		self.input = input
		self.overriden_flags = [False] #Only the inctags of variables that are changed in the loop body are activated
					#the rest is discarded

		self.visited = False #indicates if it has already been visited by isparallel(), eliminating cycles 
	def set_overriden(self, steer_level=None):
		print("DEBUG Overriding %s %s" %(self.overriden_flags, steer_level))
		if steer_level > 0:
			if len(self.overriden_flags) == 1: 
			#if we are in an if/then or if/else clause and this is the first time we override the inctag inside this clause, push the trace level (control depth) so we know when to pop
				self.overriden_flags += [steer_level]
		else:
			self.overriden_flags[0] = True

		print("DEBUG Result %s" %self.overriden_flags)
	def reset_overriden(self):
		self.overriden_flags[-1] = False
	def push_overriden(self):
		self.overriden_flags.append(self.overriden_flags[-1])
	def pop_overriden(self, steer_level):
		if len(self.overriden_flags) > 1 and self.overriden_flags[1] == steer_level:
			self.overriden_flags.pop()

	def was_overriden(self):
		return self.overriden_flags[-1] > 0 and True or False

	def isparallel(self):
		#print "inctag parallel? %s %s" %(self, self.visited)
		if self.visited:
			output = False
		else:
			self.visited = True
		
			#print "INCTAG parallel? %s"  %([p.isparallel() for p in self.input])	
			output =  len([p for p in self.input if p.isparallel()]) > 0
			self.visited = False
		#print "answer inctag parallel? %s %s %s" %(self, output, self.visited)
		return output

class InstSuper(DataNode):
	def __init__(self, number, input, n_output, inprop=None):
		self.input = input
		self.n_output = n_output
		self.number = number
		self.inprop = inprop
class InstPar(InstSuper):
	pass

class InstReturn(DataNode):
	def __init__(self, fname, expr=None, retsnd=None):
		self.fname = fname
		self.expr = expr
		#self.retsnd = FunctionDefnParam(fname, 0)
		self.retsnd = retsnd
	def __repr__(self):
		return "%sret" %self.fname

class InstCallGrp(DataNode):
	def __init__(self, fname):
		self.funcname = fname
		self.name = "%s%d" %(fname, id(self)) 


class InstCallSnd(DataNode):
	def __init__(self, fname, callgroup, signal, index):
		self.fname = fname
		self.callgroup = callgroup
		self.oper = signal
		self.index = index

class InstRetSnd(DataNode):
	def __init__(self, fname, callgroup, signal):
		self.fname = fname
		self.callgroup = callgroup
		self.oper = signal


class FunctionDefnParam(DataNode):
	def __init__(self, fname, index):
		self.name = "%s[%d]" %(fname, index)

	def __repr__(self):
		return self.name
	


class InstPort(DataNode):
	def __init__(self, instr, portnumber):
		self.number = portnumber
		self.instr = instr
		self.index = -1
		#a = all
		self.a = False
		#s = self
		self.s = False
		self.op = ""
		self.val = 0
		self.local = False
		self.starter = False
		self.last = False
		

	def set_parout(self, index, a, s, op, val, local,starter, last):
		self.index = index
		self.a = a
		self.s = s
		self.op = op
		self.val = val
		self.local = local
		self.starter = starter
		self.last = last
	def isparallel(self):
		#print "Is instport parallel? %s %s" %(self, self.instr)
		return isinstance(self.instr, InstPar)

class SteerPort(InstPort):
	def __init__(self, value, inststeer):
		InstPort.__init__(self, inststeer, value) #to set the default variables of InstPort. they're used by visitors that generate code
		self.inststeer =  inststeer
		self.port = value
	def isparallel(self):
		#print "Is steer parallel %s %s" %(self, len([p for p in self.inststeer.input if p.isparallel()]) > 0)	
		return len([p for p in self.inststeer.input if p.isparallel()]) > 0	
class SteerTrue(SteerPort):
	def __init__(self, inststeer):
		SteerPort.__init__(self, True, inststeer)
class SteerFalse(SteerPort):
	def __init__(self, inststeer):
		SteerPort.__init__(self, False, inststeer)

class ReturnPort:
	def __init__(self, fname, callgroup):
		self.fname = fname
		self.callgroup = callgroup


class DataFlowGenVisitor(Visitor):
	

	def __init__(self, asm_file, dot_file):
		self.asm_file = asm_file
		self.dot_file = dot_file
		self.errors = 0
		self.warnings = 0
		self.signals = {}
		self.signals_stack = []
		self.funcname_stack = []
		self.trace_steers = []	
		self.instructions = []
		self.in_loop_expr = False
		self.inctags_by_expr = {}
		self.steer = None
		self.inctags = []
		self.loop_level = 0
		self.n_supers = 0
		self.steers = []

	def vTranslationUnit(self, node):
		start_debug("2")
		self._visitList(node.nodes)
		start_debug("1")
		#print "Fim da criacao do grafo"
		#f self.graphviz_file != None:
		#DataFlowTrimVisitor(self.instructions)
		GraphVizVisitor(self.instructions, self.dot_file)
		TalmVisitor(self.instructions, self.asm_file)

		#if os.environ["DEBUG"] == "1":
	#		pdb.set_trace()	
	#	print node.nodes


	def vFunctionDefn(self, node):
		#start_debug("3")
		self.signals_stack.append(self.signals)
		self.signals = {}
		fname = node.name
		params = node.type.params.nodes
		
		self.funcname_stack.append(fname) 
		
		for (i, param) in map(None, range(1,len(params)+1), params):
			self.signals[param.name] = [FunctionDefnParam(fname, i)]

		self.signals["retsnd"] = [FunctionDefnParam(fname, 0)] #WORKAROUND to make retsnd match other operands in the ret instruction. This should be removed when the new tag matching scheme is implemented in Trebuchet.
		node.body.accept(self)
		
		self.signals = self.signals_stack.pop()
		self.funcname_stack.pop()	



	def vFunctionExpression(self, node):
		start_debug("3")
		fname = node.function.name
		params = node.arglist.nodes
		if params == None:
			graph_build_error("Function Calls with no parameters are not supported")
			sys.exit(1)


		callgroup = InstCallGrp(fname)
		self.instructions += [callgroup]	
		for (index, param) in map(None, range(1, len(params) + 1), params):
			signal = param.accept(self)
			callsnd = InstCallSnd(fname, callgroup, signal, index) 
			self.instructions += [callsnd]	
		
		retsnd = InstRetSnd(fname, callgroup, signal) #use the last operand for the retsnd

		self.instructions += [retsnd]
		retport = ReturnPort(fname, callgroup)
			
		return [retport]
	def vReturnStatement(self, node):
		expr = node.expr.accept(self)

		#instr = InstReturn(self.funcname_stack[-1], expr)
		instr = InstReturn(self.funcname_stack[-1], expr, self.signals["retsnd"])
		self.instructions += [instr]
	def vDeclarationList(self, node):
		for n in node.nodes:
			n.accept(self)

	def vCompoundStatement(self, node):
		node.declaration_list.accept(self)
		return node.statement_list.accept(self)	

	def vStatementList(self, node):
		#TODO: Desnecessario, verificar.
		first_instr = node.nodes[0].accept(self)
		for n in node.nodes[1:]:
			n.accept(self)
		return first_instr

	def vBinop(self, node):
		left=node.left
		right=node.right
		instr = None

		
		if node.op == "=":
			print("Creating signal %s" %left.name	)
			signals = self.signals[left.name] = right.accept(self)
			#just one signal, but the variable is named in plural because it's a list
			steer_trace_depth = len(self.trace_steers)	
			if self.loop_level > 0 and left.name in self.inctags[-1]:
				inctag = self.inctags[-1][left.name]
				inctag.set_overriden(steer_trace_depth) #mark the inctag as overriden in the current scope
			
			if steer_trace_depth > 0:
				#print "DEBUG Adding signal %s" %left.name

				self.steer.created_signals += [left.name]
	
				#print "DEBUG %s" %[st.created_signals for st in self.trace_steers]
							
		else:
			isImmediate = (not isinstance(left, cparse.Const)) and isinstance(right, cparse.Const)

			print("DEBUG Binop %s being created" %node.op)
			if isImmediate:
				instr = InstBinopI(node.op, node.type.get_dec_string()) 
				instr.input = left.accept(self)
				instr.immed = right.calculate()
			
			else:
				instr = InstBinop(node.op, node.type.get_dec_string()) 
				instr.left = left.accept(self)
				instr.right = right.accept(self)	
			self.instructions += [instr]
		
		return [instr]
		
	def vDeclaration(self, node):
		if node.is_initialized:
			node.init.accept(self)

	def vId(self, node):
		if node.name not in self.signals:
			graph_build_error("Variable %s has not been initialized." %node.name)
			sys.exit(1)
		signals = self.signals[node.name]
		
		if len(self.trace_steers) > 0: #self.conditional_trace:
			signals = self.add_steer(signals, node.name) #[self.add_steer(s) for s in signals]

		if self.in_loop_expr:
				                                                                
			#print "Id inside loop: %s == %s" %(node.name, self.signals[node.name])
			signals = self.add_inctag(node.name, signals)          #else:				
			#return list(self.signals[node.name]) #return a copy of the signals list
		#else:
			
		return signals
	def vConst(self, node):
		#print "Criando constante %s " %node.value
		instr = InstConst(node.value, node.type.type_str)	
		self.instructions += [instr]
		signals = [instr]

		if len(self.trace_steers) > 0: #self.conditional_trace:
			signals = self.add_steer(signals, instr)

		if self.in_loop_expr:
			#print "Const inside loop: %s" %node.value
			signals = self.add_inctag(instr, signals)

		self.signals[instr] = signals

		return signals


	def add_inctag(self, name, signals, inctags=None):
		if inctags == None:
			inctags = self.inctags[-1]

		if name not in inctags: 
			inctags[name] = inctag = InstIncTag(signals)
			self.instructions += [inctag]
		inctag = inctags[name]	
	
		if inctag.was_overriden():
			print("DEBUG Was overriden %s %s" %(name, inctag.overriden_flags))
			return signals
		else:
			return [inctag]


	def find_steer(self, name): #finds the steer that has the desired signal. if no steer has it, returns index 0

		i=len(self.trace_steers)
		#i=5
		found_steer = False
		
		#for st in self.trace_steers:
		#	if name in st.created_signals:
		#		found_steer = True
		#	if not found_steer:
		#		i += 5
		for st in list(reversed(self.trace_steers)):
			print("DEBUG looking for %s in %s" %(name, st.created_signals))
			if name in st.created_signals:
				found_steer = True
			if not found_steer:
				i -= 1
		#if not found_steer:
		#	return 0
		#else:
		return i
	
	def find_steer_instance(self, signal):
		s=tuple(signal)
		if s in self.steers[-1]:
			return self.steers[-1][s]
		else:
			return None

	def add_steer(self, signal, name):
		found_steer = False
		last_steer = None
		steer_index = self.find_steer(name)

		print("DEBUG adding steer to %s %d" %(name, steer_index)	)
		previous_signal = signal
		
		
		for st in self.trace_steers[steer_index:]:

			if st.expr in self.inctags_by_expr:
				print("DEBUG adding inctag to %s" %name)
				previous_signal = self.add_inctag(name, previous_signal, self.inctags_by_expr[st.expr])
			else:
				print("DEBUG NOT adding inctag to %s" %name)

			print("sinal")
			print("previous signal: %s" %previous_signal)
			last_steer = self.find_steer_instance(previous_signal)

			if last_steer != None:
				print("repetido")
				last_steer.in_then_stmt = st.in_then_stmt
					
			else:			
				last_steer = st.clone()
				self.instructions += [last_steer]
				self.steers[-1][tuple(previous_signal)] = last_steer
				


				last_steer.input = previous_signal
			previous_signal = last_steer.in_then_stmt and [last_steer.create_true()] or [last_steer.create_false()]
			print(previous_signal)

		#if steer_index == 0:
		#	self.steer.created_signals += [signal]
                                                                        
		return previous_signal
	
		
	def adjust_signals(self, s_before, s_then, s_else):
		signals = {}#semideepcopy(s_before)
		print("DEBUG Adjust signals"	)
		self.steer.in_then_stmt = True
		for name in s_then:
			if (name not in s_before) or s_then[name] != s_before[name]:
				signals[name] = list(s_then[name])

			else:
				if name in s_else and s_else[name] != s_before[name]:
					#if the then clause didn't rewrite the signal, but the else clause did
					#add steers to the original signals connecting to the True(then) port

					signals[name] = self.add_steer(s_before[name], name)
					
				else:
					signals[name] = s_before[name]
		print("DEBUG Adjust ELSE")
		self.steer.in_then_stmt = False
		for name in s_else:
			if (name not in s_before) or s_else[name] != s_before[name]:
				if name not in signals:
					signals[name] = []
				signals[name] += s_else[name]
			else:
				if name in s_then and s_then[name] != s_before[name]:
					#if the else clause didn't rewrite the signal, but the then clause did
					#add steers to the original signals conecting to the False(else) port
					signals[name] += self.add_steer(s_before[name], name) 
					
					
						
	
		#NOTE: Another implementation would be:
		# for name in signals[name]: 
		#	if s_then[name] == s_before and s_else[name] != s_before:...
	

		return signals



	def adjust_inctags(self, signals_before, signals_then):
		print("DEBUG ADJUST INCTAGS")
		
		print("DEBUG ADJUST INCTAGS %s %s %s" %(signals_before, signals_then, self.inctags[-1])	)
		self.steer.in_then_stmt = False
		for name in signals_then:
			signal_written_in_then = name not in signals_before or signals_before[name] != signals_then[name]
			if name in self.inctags[-1] and name in signals_before:
				print("DEBUG overridens %s" %self.inctags[-1][name].overriden_flags)
				self.inctags[-1][name].reset_overriden() #Reset the inctag's override flag so the signal comes from it.
				self.signals[name] = self.add_steer(signals_before[name], name)

			if name not in self.inctags[-1] and signal_written_in_then:
				self.signals[name] = self.add_steer(signals_then[name], name)
				self.inctags[-1].pop(name) #remove the inctag we just created in add_steer, so it won't receive signals_then[name] for the second time in the next loop.

				
		for name in self.inctags[-1]:
			inctag = self.inctags[-1][name]
			if (name not in signals_before) or signals_before[name] == signals_then[name]:

				self.steer.in_then_stmt = True
				signals = self.add_steer(signals_then[name], name)

			else:
				signals = signals_then[name]
			inctag.input += signals
			
			

	def push_itag_flags(self):
		if self.loop_level > 0:
			for name in self.inctags[-1]:
				inctag = self.inctags[-1][name]
				print("Pushing overridens %s %s" %(name, inctag.overriden_flags))
				inctag.push_overriden()

	def pop_itag_flags(self):
		if self.loop_level > 0:
			for name in self.inctags[-1]:
				inctag = self.inctags[-1][name]
				print("Popping overridens %s %s" %(name, inctag.overriden_flags))
				inctag.pop_overriden(len(self.trace_steers))


	def create_loop(self, expr, stmt):
		self.loop_level += 1
		self.inctags += [{}]
	
		signals_before, signals_then, signals_else = self.create_if(expr, stmt, is_a_loop=True)
		self.adjust_inctags(signals_before, signals_then)
		self.close_if()
		
		self.loop_level -= 1
		self.inctags.pop()

	#def vIfStatement(self, node):
	def create_if(self, expr, then_stmt, else_stmt=None, is_a_loop=False):
		self.steers += [{}]
		#print self.signals	
		self.in_loop_expr = is_a_loop #notify that we are accepting the expression of a loop
					 #so inctags will be added (without steers)
		steer_expr = tuple(expr.accept(self)) #convert to tuple in order to use as a dict key. 
		self.in_loop_expr = False
		if is_a_loop:
			self.inctags_by_expr[steer_expr] = self.inctags[-1] #a way to find inctags through the respective loop expression
			#TODO: eventually remove this entry from the dictionary to free space

		signals_before = semideepcopy(self.signals)

		#self.push_itag_flags() #push the overriden tags
		
		self.steer = InstSteer(steer_expr, True)
		self.trace_steers += [self.steer]
		
		then_stmt.accept(self) #Then
		
		signals_then = self.signals

		self.signals = semideepcopy(signals_before) 
		

		self.trace_steers[-1] = self.steer = InstSteer(steer_expr, False)

		self.pop_itag_flags()
		if else_stmt != None: #Else
			self.push_itag_flags()
			else_stmt.accept(self)  #rewrites self.signals for every signal rewritten in the else clause
			self.pop_itag_flags()
		
		signals_else = self.signals
		
		self.steer.created_signals = [] #clean the created signals for adjust_signals

		if self.steer.expr not in self.inctags_by_expr: #if this is a loop condition, we will use adjust_inctags instead of adjust_signals

			self.signals = self.adjust_signals(signals_before, signals_then, signals_else)
		
		#self.trace_steers = self.trace_steers[:-1] #TODO: self.trace_steers.pop()
		#self.steer = len(self.trace_steers) > 0 and self.trace_steers[-1] or None	
		
			
		return (signals_before, signals_then, signals_else)

	def close_if(self):
		self.trace_steers = self.trace_steers[:-1] #TODO: self.trace_steers.pop()
		self.steer = len(self.trace_steers) > 0 and self.trace_steers[-1] or None
		self.steers.pop()	

	def vIfStatement(self, node):

		expr = node.expr
		then_stmt = node.then_stmt
		if node.else_stmt.is_null():
			else_stmt = None
		else:
			else_stmt = node.else_stmt
		self.create_if(expr, then_stmt, else_stmt)
		self.close_if()

	def vWhileLoop(self, node):
		self.create_loop(node.expr, node.stmt)
		


	def vForLoop(self, node):
		pass

	def check_mytid_local(self, inputs):
		"""
		This method checks the correctness of local inputs and its mytid
		expression.
		
		All locals need a mytid expression.
		
		In the case where there is more than one local input in a 
		parallel super-instruction, if mytid expressions with both + and
		- operands were allowed, there could be circular dependencies 
		between different tasks of this instruction.
		"""
		positive = False
		negative = False
		
		for n in inputs:
			if n.local:
				if n.op == "":
					graph_build_error("Error: Local input variable %s must have mytid expression associated (line %d)." % (n.name, n.lineno))
					sys.exit(1)
				if n.op == "+":
					if negative:
						graph_build_error("Error: Local input variable %s has a possible circular dependence with other local (line %d)." % (n.name, n.lineno))
						sys.exit(1)
					positive = True
				if n.op == "-":
					if positive:
						graph_build_error("Error: Local input variable %s has a possible circular dependence with other local (line %d)." % (n.name, n.lineno))
						sys.exit(1)
					negative = True
		return (positive or negative)

	def reorderinputs(self, inputs):
		"""
		This method reorders inputs, placing non-local inputs first and 
		then local inputs in a descendent order so that locals can be 
		ommited at code generated for certain tasks. For example, if 
		local.d::(mytid-2) is one of the inputs of a super, it means 
		that tasks 0 and 1 will	have d as input. 
		
		Also, starter inputs are placed last. Those inputs are just to
		define control. User can not use them to send data
		"""
		end=len(inputs)
		for i in range(0,len(inputs)):
			if inputs[0].starter:
				x=inputs[0]
				for j in range(1,end):
					inputs[j-1]=inputs[j]
				end-=1
				inputs[end]=x
			
		for i in range(0,(end-1)):
			if inputs[i].local:
				for j in range(i+1,end):
					if (not inputs[j].local) or (inputs[j].local and (eval(inputs[j].op+str(inputs[j].val)) > eval(inputs[i].op+str(inputs[i].val)))):
						x=inputs[j]
						inputs[j]=inputs[i]
						inputs[i]=x
			
					
				 
	
	#TODO: 1) redundant code. FIX IT
	#      2) The visitors for InstSuper and InstPar should be separated	
	def vSuperInstruction(self, node):
		#start_debug("SUPER")
		inputs = []
		inprop = []
		
		#print node.inp.nodes
		#start_debug("SUPER")

		#first reorder inputs (put locals at the end) and check for invalid mytid expressions
		has_locals = self.check_mytid_local(node.inp.nodes)
		
		if has_locals:
			self.reorderinputs(node.inp.nodes)

		for n in node.inp.nodes:
			#if input is not LOCAL, proceed as usual:
			tmp=[]
			if not n.local:
				x = n.accept(self)
				for i in range(0,len(x)):
					if isinstance(x[i], InstPort):
						t=InstPort(x[i].instr, x[i].number) #why are you creating another instance? can't you use the same?
						t.set_parout(n.index, n.a, n.s, n.op, n.val, n.local, n.starter, n.last)
						tmp.append(t)
					else:
						tmp.append(None)
			else:
				#input will be created when looking at outputs
				x=n
				tmp.append(None)
			inputs += [x]
			inprop += [tmp]
		self.n_supers += 1

		#TODO: MAKE IT CLEANER!!!!
		if isinstance(node, cparse.ParInstruction):
			instr = InstPar(self.n_supers, inputs, len(node.out.nodes), inprop)
		else:
			if has_locals:
				graph_build_error("Error: super single can not have local inputs (line %d)." % node.inp.nodes[0].lineno)
				sys.exit(1)
			instr = InstSuper(self.n_supers, inputs, len(node.out.nodes), inprop)
	
		i = 0
		for n in node.out.nodes:
			instport = InstPort(instr, i)
			
			k = 0
				
			#signals to local inputs are discovered when looking at local outputs
			for inp in instr.input:
				if isinstance(inp, cparse.Id):
					if n.name == inp.name:
						instr.input[k]=[InstPort(instr, i)]
						inprop[k]=[InstPort(instr, i)]
						inprop[k][0].set_parout(inp.index, inp.a, inp.s, inp.op, inp.val, inp.local, inp.starter, n.last)
				k += 1
					
			i += 1
			self.signals[n.name] = [instport]
		
			if self.loop_level > 0 and n.name in self.inctags[-1]:
				print("DEBUG has inctag %s" %n.name)
				inctag = self.inctags[-1][n.name]
				inctag.set_overriden(len(self.trace_steers))
			
			if len(self.trace_steers) > 0:
				print("DEBUG Super Adding signal %s" %n.name)
		                                                                                         
				self.steer.created_signals += [n.name]
		
		
		
			print("DEBUG signals %s" %self.signals	)
	
		#check if all local inputs have been declared as outputs
		for inp in instr.input:
				if isinstance(inp, cparse.Id):	
					graph_build_error("Error: Local input variable %s is not produced at the same super-instruction (line %d)." % (inp.name, inp.lineno))
					sys.exit(1)
		#print inputs
		self.instructions += [instr]

			
	vParInstruction	= vSuperInstruction






		

