# clipsruby

## Description

Use the CLIPS programming language from within Ruby

## Example

```ruby
require "clipsruby"

class CLIPS::Environment
	def foo(a, b=2)
		puts "a: #{a}"
		puts "b: #{b}"
		a + b
	end
end

env = CLIPS::Environment.new
puts "foo: #{env.add_udf(:foo)}"
env.build("(deftemplate foo (slot bar) (slot baz) (slot bat) (slot buz))")
env.assert_string("(foo (bar 123) (baz something) (bat 4.56) (buz fizz))")
fact = env.assert_hash(:foo, bar: 789, baz: :thing, bat: 3.8, buz: "another thing")
p fact.deftemplate_name
env.facts
env.build("(defrule foo (foo (bar ?bar) (bat ?bat)) => (println \"bar + bat: \" (foo ?bar)))")
env.run(1)
CLIPS::Environment.run(env)
```
