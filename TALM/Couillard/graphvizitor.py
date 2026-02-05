#Graphviz code generator for the dataflow graph
#Author: Tiago A.O.A.
import cparse
from cvisitors import Visitor
import flow
class GraphVizVisitor(Visitor):
	def __init__(self, instructions, dot_file):
		self.edges =[]
		self.dot_file = dot_file
		self.p("digraph G{")
		for inst in instructions:
			inst.accept(self)
	
		for edge in self.edges:
			self.p("\t%s" % edge)
		self.p("}")

	def p(self, str):
                self.dot_file.write(str + "\n")

	def print_edge(self, a, b, tailport=None, headport=None):
		
		if isinstance(a, flow.SteerPort):

			if isinstance(a, flow.SteerTrue):
				headport="sw"
			if isinstance(a, flow.SteerFalse):
				headport="se"
			a = a.inststeer
		if isinstance(a, flow.InstPort):
			a = a.instr

		edgestr = "%s -> %s" %(a, b)
		edgestr += " [tailport=%s, headport=%s];" %(headport and headport or 's', tailport and tailport or 'n')

		self.edges += [edgestr]
		
	def vInstConst(self, node):
		self.p('\tnode [shape=box, style=rounded];')
		self.p('\tnode [label="const #%s"] %s;' %(node.value, node))

	def vInstBinop(self, node):
		self.p('\tnode [shape=box, style=rounded];')
		self.p('\tnode [label="%s"] %s;' %(node.op, node))
		for input in node.left:
			self.print_edge(input, node, tailport="nw")

		for input in node.right:
			self.print_edge(input, node, tailport="ne")

	def vInstBinopI(self, node):
		self.p('\tnode [shape=box, style=rounded];')
		if node.immed>=0:
			self.p('\tnode [label="%s%s"] %s;' %(node.op, str(node.immed), node))
		else:
			self.p('\tnode [label="%s(%s)"] %s;' %(node.op, str(node.immed), node))
		for input in node.input:
			self.print_edge(input, node, tailport="n")


	def vInstSteer(self, node):
		self.p('\tnode [shape=triangle style=solid];')
		self.p('\tnode [label="T   F"] %s' %node)
	

		for input in node.input:	
			self.print_edge(input, node)
		for condition in node.expr:
			self.print_edge(condition, node, tailport="nw")



	def vInstIncTag(self, node):
		self.p('\tnode [shape=circle];')
		self.p('\tnode [label="IT"] %s' %node)
		for input in node.input:
			self.print_edge(input, node)


	def vInstSuper(self, node):
		self.p('\tnode [label="S%d" shape=rectangle style=solid] %s;' %(node.number, node))
		for input in node.input:
			for source in input: 
				self.print_edge(source, node)

		
