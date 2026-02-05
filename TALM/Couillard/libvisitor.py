#Graphviz code generator for Trebuchet Lib
#Author: Leandro Augusto Justen Marzulo
import cparse
import string

from cvisitors import Visitor

class LibPrinterVisitor(Visitor):
    def __init__(self, lib_file):
        #TODO: add long int and void pointer to list of types
        self.treb_types = {"int":"i", "float":"f", "double":"d", "char":"c", "long int": "li", "long long int":"lli", "pointer":"p"}
        self.lib_file = lib_file
        self.n_supers = 1
        self.p('#include "queue.h"')
        self.p('#include "interp.h"')
        self.p("extern int superargc;")
        self.p("extern char ** superargv;")
        Visitor.__init__(self)

    def p(self, str):
        self.lib_file.write(str + "\n")

    def pinputs(self, nodes):
        printed = 0;
        a=0;
        for node in nodes:
            m_type = node.type.get_dec_string()
            if string.find(m_type, "*")>=0:
                t_type="pointer"
            else:
                t_type=m_type
            if t_type in self.treb_types:
                if node.a:
                    self.p("\t%s* %s;" % (m_type, node.name))
                    self.p("\t%s = (%s*) malloc(treb_get_n_tasks()*sizeof(%s));" % (node.name, m_type, m_type))
                    self.p("\tint mytask;")
                    self.p("\tfor(mytask=0; mytask<treb_get_n_tasks();mytask++){")
                    if a:
                        if printed:
                            self.p("\t\t%s[mytask]=oper[%d+(%d*treb_get_n_tasks())+mytask]->value.%s;" % (node.name, printed, a, self.treb_type[t_type]))
                        else:
                            self.p("\t\t%s[mytask]=oper[(%d*treb_get_n_tasks())+mytask]->value.%s;" % (node.name, a, self.treb_type[t_type]))
                    else:
                        if printed:
                            self.p("\t\t%s[mytask]=oper[%d+mytask]->value.%s;" % (node.name, printed, self.treb_types[t_type]))
                        else:
                            self.p("\t\t%s[mytask]=oper[mytask]->value.%s;" % (node.name, self.treb_types[t_type]))
                    self.p("\t}")
                    a+=1
                elif node.starter:
                    pass
                else:
                    if a:
                        oper = "oper[%d+(%d*treb_get_n_tasks())]" % (printed, a)
                    else:
                        oper = "oper[%d]" % printed
                    #since local inputs are not present in some tasks, we need to check for references to null pointers
                    buf = ""
                    if node.local:
                        buf += "\t%s %s;\n" % (m_type, node.name) #declaration
                        buf += "\tif (%s)\n" %  oper #null pointer check
                        buf += "\t\t%s" % node.name #atribution
                    else:
                        buf += "\t%s %s " % (m_type, node.name) #declaration and atribution
                    buf += " = %s->value.%s;" % (oper, self.treb_types[t_type])
                    self.p(buf)
                    printed+=1
    
    def pdeclarations(self, inps, outs):
        for o in outs:
            found=0
            for i in inps:
                if o.name == i.name:
                    found=1
                    break
            if not(found):
                self.p("\t%s %s;" % (o.type.get_dec_string(), o.name))

    def poutputs(self, nodes):
        printed = 0
        for node in nodes:
            m_type = node.type.get_dec_string()
            if string.find(m_type, "*")>=0:
                t_type="pointer"
            else:
                t_type=m_type
            if t_type in self.treb_types:
                self.p("\tresult[%d].value.%s = %s;" % (printed, self.treb_types[t_type], node.name))
                printed+=1

    def vSuperInstruction(self, node):
        self.p("void super%d(oper_t **oper, oper_t *result){" % self.n_supers)
        self.pdeclarations(node.inp.nodes, node.out.nodes)
        self.pinputs(node.inp.nodes)
        self.p("%s" % node.body)
        self.poutputs(node.out.nodes)
        self.p("}")
        self.n_supers+=1
    
    def vBlock(self, node):
        self.p("%s" % node.body)

    def vNodeList(self, node):
        #self.p("List")
                self._visitList(node.nodes)

    def vNode(self, node):
        #self.p("Node")
        #print dir(node)
        for attr in dir(node):
            #print attr
            #print type(attr)
            if (isinstance(getattr(node, attr), cparse.Node) or isinstance(getattr(node, attr), cparse.NodeList)): 
                getattr(node, attr).accept(self)

    def vId(self, node):
        pass
