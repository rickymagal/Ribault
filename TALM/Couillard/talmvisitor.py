#Generate TALM assembly code
#authors: Tiago A.O.A. and Leandro J. Marzulo

import cparse
from cvisitors import Visitor
import flow


class DataFlowTrimVisitor(Visitor):
	

	def __init__(self, instructions):
		trimmed=True
		while trimmed:
			trimmed=False
			k=0
			for i in range(0,len(instructions)):
				if not isinstance(instructions[k], flow.InstSuper):
					self.inst=instructions[k]
					self.count=0
					for j in instructions:
						j.accept(self)
					if self.count==0:
						print (instructions[k])
						instructions.remove(self.inst)
						trimmed=True
					else:
						k=k+1
				else:
					k=k+1
	def get_inst(self, src):
		if isinstance(src, flow.InstPort):
			return src.instr
			
		if isinstance(src, flow.SteerPort):
			return src.inststeer
			
		return src
	def vInstBinop(self, node):
		for l in node.left:
			if self.get_inst(l)==self.inst:
				self.count=self.count+1
		for r in node.right:
			if self.get_inst(r)==self.inst:
				self.count=self.count+1

	def vInstBinopI(self, node):
		for i in node.input:
			if self.get_inst(i)==self.inst:
				self.count=self.count+1


	def vInstSteer(self, node):
		for e in node.expr:
			if self.get_inst(e)==self.inst:
				self.count=self.count+1
		for i in node.input:
			if self.get_inst(i)==self.inst:
				self.count=self.count+1


	def vInstIncTag(self, node):
		for i in node.input:
			if self.get_inst(i)==self.inst:
				self.count=self.count+1

	def vInstSuper(self, node):
		for inp in node.input:
			for i in inp:
				if self.get_inst(i)==self.inst:
					self.count=self.count+1

class TalmVisitor(Visitor):
	

	def __init__(self, instructions, asm_file):
		self.ops = {"+":"add", "-":"sub", "*":"mul", "/":"div", "&&": "and", "<":"lthan", ">": "gthan"}
		self.types = {"int":"", "float":"f", "double":"d"}
		self.asm_file = asm_file
		self.edges = []

		for inst in instructions:
			inst.accept(self)

	def p(self, str):
                self.asm_file.write(str + "\n")

	def print_source(self, src):
		if isinstance(src, flow.InstPort):
			if isinstance(src, flow.SteerPort):
	 			port = isinstance(src, flow.SteerTrue) and "t" or "f"
			else:
				port = "%d" %src.number

			if src.index > -1:
				return "%s_t%d.%s" %(src.instr, src.index, port)
			elif src.a == 1:
				return "%s_t${0..NUM_TASKS-1}.%s" %(src.instr, port)
			elif src.last:
				return "%s_t${NUM_TASKS-1..NUM_TASKS-1}.%s" %(src.instr, port)
			elif src.op != "":
				x=("(i%s(%d%%NUM_TASKS))%%NUM_TASKS") % (src.op, src.val)
				return "%s_t${%s}.%s" %(src.instr, x, port)
			elif src.s == 1 or src.isparallel():
                		return "%s_t${i}.%s" %(src.instr, port)
			else:
				return "%s.%s" %(src.instr, port)
		
		#if isinstance(src, flow.SteerPort):
		#	if isinstance(src, flow.SteerTrue):
		#		v="t"
		#	else:
		#		v="f"
		#	if (src.inststeer.isparallel()):
		#		return "%s_t${i}.%s" %(src.inststeer, v)
		#	else:
		#		return "%s.%s" %(src.inststeer, v)

		if isinstance(src, flow.ReturnPort):
			return "%sret.%s" %(src.fname, src.callgroup.name)

		if src.isparallel():
			return "%s_t${i}" %src
		else:

			return src

	def  get_sources(self, src):
		sources = ""
		
		if len(src) > 1:
			sources += "[%s" %(self.print_source(src[0]))
			for s in src[1:]:
				sources += ", %s" % self.print_source(s)
			sources += "]"
		else:
			sources = self.print_source(src[0])

		return sources
		
	def get_opfield(self, instr):
		#add a prefix to the instruction's operator field if it is parallel
		if instr.isparallel():
			return "{i=0..NUM_TASKS-1} ", "_t${i}"
		else:
			return "", ""
		


	def fix_inprops(self, input, inprop):
		for i in range(0,len(input)):
			for j in range(0,len(input[i])):
				if isinstance(input[i][j], flow.InstPort):
					input[i][j].set_parout(inprop[i][j].index, inprop[i][j].a, inprop[i][j].s, inprop[i][j].op, inprop[i][j].val, inprop[i][j].local, inprop[i][j].starter, inprop[i][j].last)
	
	def check_local_inputs(self, inplist):
		"""
		This method checks if there are local inputs and if the operator
		used in their mytid expressions are + or -. It also returns a
		list of all values used on local mytid expressions.
		"""
		has_locals = False
		positive = False
		local_vals = []
		starters=0
		for input in inplist:
			#Locals will always have one src (we never use [] construct). We only need to worry with scr 0.
			if isinstance(input[0], flow.InstPort) and input[0].local:
				has_locals = True
				if input[0].op == "+":
					positive = True
				local_vals += [input[0].val]
			if isinstance(input[0], flow.InstPort) and input[0].starter:
				starters+=1
		return (has_locals, positive, local_vals, starters)
		
	
	def vInstConst(self, node):
		constinstr = node.type == 'int' and 'const' or 'fconst'
		self.p('%s %s, %s' %(constinstr, node, node.value))

	def vInstBinop(self, node):
		if node.typestr in self.types:
			if node.op in self.ops:
				left = self.get_sources(node.left)
				right = self.get_sources(node.right)
				self.p("%s%s %s, %s, %s " %(self.types[node.typestr], self.ops[node.op], node, left, right))
			else:
				raise NotImplementedError()
		else:
			raise NotImplementedError()

	def vInstBinopI(self, node):
		if node.typestr in self.types:
			if node.op in self.ops:
				inp = self.get_sources(node.input)
				immed = str(node.immed)
				self.p("%s%si %s, %s, %s " %(self.types[node.typestr], self.ops[node.op], node, inp, immed))
			else:
				print("ERROR: %s not implemented" %(node.op))
				raise NotImplementedError()
		else:
			raise NotImplementedError()



	def vInstSteer(self, node):
		prefix, sufix = self.get_opfield(node) 
		self.p ('%ssteer %s%s, %s, %s' % (prefix, node, sufix, self.get_sources(node.expr), self.get_sources(node.input)))


	def vInstIncTag(self, node):
		prefix, sufix = self.get_opfield(node)
		self.p('%sinctag %s%s, %s' % (prefix, node, sufix, self.get_sources(node.input)))
	
	
	def vInstPar(self, node):
		self.fix_inprops(node.input, node.inprop)
		has_locals, positive, local_vals, starters = self.check_local_inputs(node.input)
		#if we have locals, tasks of the same super will have different inputs.
		if has_locals:
			if positive:
				#in case we have mytid expressions with + operator, last tasks will have less inputs
				t=0+starters
				local_vals += [0]
				begin="0"
				for k in local_vals:
					self.p('placeinpe(%s,"DYNAMIC")' % begin)
					out = "{i=%s..NUM_TASKS-%d} superi %s_t${i}, %d, %d"  %(begin, (1+k), node, node.number, node.n_output)
					begin = "NUM_TASKS-%d" % k
					for i in range(0, len(node.input)-t):
						out += ", %s" %self.get_sources(node.input[i])
					t+=1
					#TODO: THAT'S UGLY!! 
					if k==0:
						for i in range(len(node.input)-starters, len(node.input)):
							out += ", %s" %self.get_sources(node.input[i])
					out += ', ${i}'
					self.p(out)
			else:
				#in case we have mytid expressions with - operator, first tasks will have less inputs
				t=len(local_vals)+starters
				local_vals = [0] + local_vals
				for k in range(0,len(local_vals)):
					self.p('placeinpe(%d,"DYNAMIC")' % local_vals[k])
					if k==(len(local_vals)-1):
						end="NUM_TASKS-1"
					else:
						end=str(local_vals[k+1]-1)
					out = "{i=%d..%s} superi %s_t${i}, %d, %d"  %(local_vals[k], end, node, node.number, node.n_output)
					for i in range(0, len(node.input)-t):
						out += ", %s" %self.get_sources(node.input[i])
					t-=1
					if k==0:
						for i in range(len(node.input)-starters, len(node.input)):
							out += ", %s" %self.get_sources(node.input[i])
					out += ', ${i}'
					self.p(out)
			self.p('placeinpe(0,"DYNAMIC")')
		else:
			self.p('placeinpe(0,"DYNAMIC")')
			out = "{i=0..NUM_TASKS-1} superi %s_t${i}, %d, %d"  %(node, node.number, node.n_output)
			for input in node.input:
				out += ", %s" %self.get_sources(input)
			out += ', ${i}'
			self.p(out)

	def vInstSuper(self, node):
		out = "super %s, %d, %d" %(node, node.number, node.n_output)
		self.fix_inprops(node.input, node.inprop)
		for input in node.input:
			out += ", %s" %self.get_sources(input)
		self.p(out)
		
	def vInstReturn(self, node):
		self.p("ret %s, %s, %s" %(node, self.get_sources(node.expr), self.get_sources(node.retsnd)))   


	def vInstCallGrp(self, node):
		self.p('callgroup("%s", "%s")' %(node.name, node.funcname))

	def vInstCallSnd(self, node):
		sources = self.get_sources(node.oper)
		self.p("callsnd %s[%d], %s, %s" %(node.fname, node.index, sources, node.callgroup.name))
		
	def vInstRetSnd(self, node):
		sources = self.get_sources(node.oper)
		self.p("retsnd %s[0], %s, %s" %(node.fname, sources, node.callgroup.name))

