# encoding: utf-8
require "strscan"
require "pp"

module Unlambda
	module_function
	def gen_node_class(name, members)
		klass = Struct.new(*members)
		klass.class_eval do
			define_method :node_type do
				name
			end
		end
		klass
	end

	PrimaryNode = gen_node_class(:primary, [:name])
	CallNode = gen_node_class(:call, [:func, :arg])
	LambdaNode = gen_node_class(:lambda, [:arg_name, :body])
	ArgNode = gen_node_class(:arg, [:name])

	def parse(str)
		Parser.new(str).parse()
	end
	
	def parse_algol_style(str)
		AlgolStyleParser.new(str).parse()
	end

	class ParseError < StandardError; end

	class Parser
		def initialize(str)
			@scanner = StringScanner.new(str)
		end
		
		def parse
			node = parse_expr()
			if @scanner.rest?
				error "extra tokens"
			end
			node
		end
		
		def parse_expr
			s = @scanner
			case
			when s.scan(/\(/)
				r = parse_expr()
				if not s.scan(/\)/)
					error "expecting ')'"
				end
				r
			when s.scan(/\w/i)
				PrimaryNode.new(s[0])
			when s.scan(/`/)
				func = parse_expr()
				arg = parse_expr()
				CallNode.new(func, arg)
			when s.scan(/^(\w)/)
				arg_name = s[1]
				LambdaNode.new(arg_name, parse_expr())
			when s.scan(/\$(\w)/)
				name = s[1]
				ArgNode.new(name)
			else
				error "unknown tokens"
			end
		end
		
		def error(message)
			raise ParseError, "#{message}: #{@scanner.rest}"
		end
	end

	class AlgolStyleParser < Parser
		def parse_expr
			s = @scanner
			node = parse_primary()
			while s.scan(/\(/)
				arg_node = parse_expr()
				if not s.scan(/\)/)
					error "expecting ')'"
				end
				node = CallNode.new(node, arg_node)
			end
			node
		end
		
		def parse_primary
			s = @scanner
			case
			when s.scan(/\w/)
				PrimaryNode.new(s[0])
			when s.scan(/\$(\w)/)
				name = s[1]
				ArgNode.new(name)
			when s.scan(/\(/)
				node = parse_expr()
				if not s.scan(/\)/)
					error "expecting ')'"
				end
				node
			when s.scan(/->(\w)\{/)
				node = LambdaNode.new(s[1], parse_expr())
				if not s.scan(/\}/)
					error "expecting '}'"
				end
				node
			else
				error "expecting primary expression"
			end
		end
	end

	def lambda_to_ski(node)
		LambdaToSKIConverter.new(node).convert()
	end
	
	def convert(code)
		node_to_str(lambda_to_ski(parse(code)))
	end

	# convertとかは元のnodeを使いまわさず全nodeをコピーする方針でいく
	
	class LambdaToSKIConverter
		def initialize(node)
			@root = node
		end
		
		def convert()
			convert_node(@root)
		end
		
		def convert_node(node)
			case node.node_type
			when :primary
				node.dup
			when :call
				CallNode.new(convert_node(node.func), convert_node(node.arg))
			when :lambda
				convert_node_in_lambda(node.arg_name, node.body)
			when :arg
				raise "arg node without lambda"
			else
				raise "unknown node: #{node.node_type}"
			end
		end
		
		def convert_node_in_lambda(arg_name, node)
			case node.node_type
			when :primary
				new_k_call(node.dup)
			when :call
				arg1 = convert_node_in_lambda(arg_name, node.func)
				arg2 = convert_node_in_lambda(arg_name, node.arg)
				new_s_call(arg1, arg2)
			when :lambda
				node = convert_node_in_lambda(node.arg_name, node.body)
				convert_node_in_lambda(arg_name, node)
			when :arg
				if node.name == arg_name
					PrimaryNode.new("i")
				else
					new_k_call(node.dup)
				end
			else
				raise "unknown node: #{node.node_type}"
			end
		end
		
		def new_k_call(node)
			CallNode.new(PrimaryNode.new("k"), node)
		end
		
		def new_s_call(arg1, arg2)
			CallNode.new(CallNode.new(PrimaryNode.new("s"), arg1), arg2)
		end
	end
	
	def optimize(node)
		Optimizer.new(node).optimize()
	end
	
	
	# Optimizerのつもりで作っているけど今の状態では
	# LambdaToSKIConverterで生成されたのを元に戻すconverterといった方が近い
	class Optimizer
		def initialize(node)
			@root = node
		end
		
		def optimize()
			replace_arg_name(optimize_node(@root))
		end
		
		def optimize_node(orig_node)
			node = orig_node
			while true
				case node.node_type
				when :primary, :arg
					node = node.dup
				when :call
					node = CallNode.new(optimize_node(node.func), optimize_node(node.arg))
				when :lambda
					node = LambdaNode.new(node.arg_name, optimize_node(node.body))
				else
					raise "unsupported node: #{node.node_type}"
				end
				
				node, action = optimize_node0(node)
				if action == :break
					break
				end
			end
			node
		end
		
		def optimize_node0(node)
			while true
				if m = match(node, "i(x:*)")
					node = m[:x]
					next
				end
				if m = match(node, "k(x:*)(y:*)") and pure?(m[:y])
					node = m[:x]
					next
				end
				if m = match(node, "s(x:*)(y:*)")
					name = Object.new
					node = LambdaNode.new(
					 name,
					 CallNode.new(
					  CallNode.new(m[:x], ArgNode.new(name)),
					  CallNode.new(m[:y], ArgNode.new(name))))
					return [node, :redo]
				end
				if m = match(node, "(func:->a{*})(arg:*)") and
				   pure?(m[:arg]) and m[:arg].node_type != :lambda
					node = replace_arg(m[:func].arg_name, m[:func].body, m[:arg])
					return [node, :redo]
				end
				# どの最適化にもマッチしなければループ終了
				break
			end
			return [node, :break]
		end
		
		def replace_arg(arg_name, root, arg_value_node)
			deep_copy_node(root) do |node, recur|
				if node.node_type == :arg and node.name == arg_name
					arg_value_node
				else
					nil
				end
			end
		end
		
		def replace_arg_name(root)
			# key: original name, value: new name stack
			arg_name_table = {}
			next_name = "A"
			
			deep_copy_node(root) do |node, recur|
				if node.node_type == :lambda
					name = next_name
					next_name = next_name.succ()
					(arg_name_table[node.arg_name] ||= []).push(name)
					new_node = LambdaNode.new(name, recur.(node.body))
					arg_name_table[node.arg_name].pop()
					new_node
				elsif node.node_type == :arg
					name = (arg_name_table[node.name] || []).last
					raise "arg name #{node.name.inspect} not found" if name.nil?
					ArgNode.new(name)
				else
					nil
				end
			end
		end
		
		def deep_copy_node(root)
			copy_node = ->(node, recur) {
				case node.node_type
				when :primary, :arg
					node.dup
				when :call
					CallNode.new(recur.(node.func), recur.(node.arg))
				when :lambda
					LambdaNode.new(node.arg_name, recur.(node.body))
				else
					raise "unknown node: #{node.node_type}"
				end
			}
			recur = ->(node) {
				yield(node, recur) || copy_node.(node, recur)
			}
			recur.(root)
		end
		
		# 式が副作用を持っていたらfalse
		# とりあえず今は常にtrueを返す
		def pure?(node)
			true
		end
		
		def match(node, pattern)
			NodePatternMatcher.new(pattern).match(node)
		end
	end
	
	PatternLabelNode = gen_node_class(:pattern_label, [:name, :node])
	PatternAllNode = gen_node_class(:pattern_all, [:_dummy])
	
	class PatternParser < AlgolStyleParser
		def parse_primary()
			s = @scanner
			if s.scan(/(\w+):/)
				label = s[1].intern
			else
				label = nil
			end
			if s.scan(/\*/)
				node = PatternAllNode.new()
			else
				node = super
			end
			if label
				node = PatternLabelNode.new(label, node)
			end
			node
		end
	end
	
	class NodePatternMatcher
		def initialize(str)
			@pattern = PatternParser.new(str).parse()
			@result = {}
			@arg_name_stack = ArgNameStack.new
			@pattern_arg_name_stack = ArgNameStack.new
		end
		
		def match(node)
			matched = match0(node, @pattern)
			matched ? @result : nil
		end
		
		def match0(node, pattern_node)
			label, pattern_node = get_label(pattern_node)
			@result[label] = node if label
			if pattern_node.node_type == :pattern_all
				return true
			end
			if node.node_type != pattern_node.node_type
				return false
			end
			case node.node_type
			when :primary
				node.name == pattern_node.name
			when :call
				match0(node.func, pattern_node.func) and
				match0(node.arg, pattern_node.arg)
			when :lambda
				@arg_name_stack.push(node)
				@pattern_arg_name_stack.push(pattern_node)
				r = match0(node.body, pattern_node.body)
				@arg_name_stack.pop()
				@pattern_arg_name_stack.pop()
				r
			when :arg
				# 何段階上の関数の引数かで比較する
				level1 = @arg_name_stack.get_level(node.name)
				level2 = @pattern_arg_name_stack.get_level(pattern_node.name)
				level1 == level2
			else
				raise "unsupported node type: #{node.node_type}"
			end
		end
		
		def get_label(node)
			if node.node_type == :pattern_label
				[node.name, node.node]
			else
				[nil, node]
			end
		end
	end
	
	class ArgNameStack
		def initialize
			@stack = []
		end
		
		def push(func_node)
			@stack.push(func_node.arg_name)
		end
		
		def pop
			@stack.pop
		end
		
		def get_level(name)
			@stack.reverse_each.with_index do |n, i|
				return i if n == name
			end
			nil
		end
	end

	def node_to_code(node)
		return "<NIL>" if node == nil

		case node.node_type
		when :primary
			node.name
		when :call
			func = node_to_code(node.func)
			arg = node_to_code(node.arg)
			"(`#{func}#{arg})"
		when :lambda
			"(^#{node.arg_name}#{node_to_code(node.body)})"
		when :arg
			"$#{node.name}"
		else
			raise "unknown node: #{node.node_type}"
		end
	end
	
	def node_to_algol_style_code(node)
		return "<NIL>" if node == nil

		case node.node_type
		when :primary
			node.name
		when :call
			func = node_to_algol_style_code(node.func)
			arg = node_to_algol_style_code(node.arg)
			"#{func}(#{arg})"
		when :lambda
			"(->#{node.arg_name}{ #{node_to_algol_style_code(node.body)} })"
		when :arg
			"$#{node.name}"
		else
			raise "unknown node: #{node.node_type}"
		end
	end

	def algol_style_to_unlambda_style(str)
		node_to_code(AlgolStyleParser.new(str).parse())
	end

	def fair_code(str)
		node_to_code(parse(str))
	end
	
	def change_inspect(obj, name)
		(class << obj; self; end).class_eval do
			define_method :inspect do
				name
			end
		end
		obj
	end

	S = ->(f){ ->(g){ ->(x){ f[x][g[x]] } } }
	K = ->(x){ ->(y){ x } }
	I = ->(x){ x }

	change_inspect S, "s"
	change_inspect K, "k"
	change_inspect I, "i"
	
	BUILTIN_FUNCS = {
		"s" => S,
		"k" => K,
		"i" => I
	}
	
	def evaluate(node)
		case node.node_type
		when :primary
			val = BUILTIN_FUNCS[node.name]
			raise "unsupported builtin func: #{node.name}" if val.nil?
		when :call
			func = evaluate(node.func)
			arg = evaluate(node.arg)
			func.(arg)
		else
			raise "unsupported node: #{node.node_type}"
		end
	end
	
	# 関数の動作確認用
	# > a, b, c = gen_procfakes(3)
	# > S.(a).(b).(c)
	# a(c)
	# b(c)
	# a(c)(b(c))
	# => a(c)(b(c))
	# みたいになってとても便利ですね
	def gen_procfakes(num)
		s = "a"
		l = []
		num.times do |i|
			l << ProcFake.new(s)
			s = s.succ
		end
		l
	end
	
	class ProcFake
		def initialize(name)
			@name = name
		end
		
		def call(arg)
			s = "#{self.inspect()}(#{arg.inspect()})"
			puts s
			ProcFake.new(s)
		end
		
		alias [] call
		
		def inspect
			@name
		end
	end
	
	def church_to_number(c)
		v = 0
		c.(->(x){ v += 1 }).(nil)
		v
	end
end


if $0 == __FILE__
	include Unlambda
	
	tree = lambda_to_ski(parse_algol_style("->A{->B{->F{->X{$A($F)($B($F)($X))}}}}"))
	puts node_to_algol_style_code(tree)
	optimized_tree = optimize(tree)
	puts node_to_algol_style_code(optimized_tree)
end
