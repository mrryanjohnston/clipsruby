# clipsruby

## Description

Use the CLIPS programming language from within Ruby

## Installation/Usage

This is available as a Ruby gem:

```
gem install clipsruby
```

From there, you should be able to `require "clipsruby"` in your code
and use the below API as described.

## API

### `CLIPS.create_environment`
### `CLIPS::Environment.new`

Create a new CLIPS environment in which you may define Rules,
assert Facts, and instantiate Objects.

```ruby
env = CLIPS.create_environment
env2 = CLIPS::Environment.new
```

### `CLIPS::Environment.batch_star`
### `env.batch_star`

Pass the path to a file that will be "batched" into the environment,
allowing many CLIPS commands to be entered at once.

```ruby
CLIPS::Environment.batch_star(env, "./my-batch.bat")
env.batch_star("./my-batch.bat")
```

### `CLIPS::Environment.save`
### `env.save`

Pass the path in which we'll save the environment constructs.

```ruby
CLIPS::Environment.save(env, "./my-save.clp")
env.save("./my-save.clp")
```

### `CLIPS::Environment.load`
### `env.load`

Pass the path to a file that will load the constructs stored in the file
into the environment. Remember to `reset` the environment if you need.

```ruby
CLIPS::Environment.load(env, "./my-load.clp")
env.load("./my-load.clp")
```

### `CLIPS::Environment.save_facts`
### `env.save_facts`

Pass the path in which we'll save the environment facts.
The third argument is optional, and determines whether to store the visible or local facts.
By default, it'll store local.

```ruby
CLIPS::Environment.save_facts(env, "./my-save-facts.clp")
env.save_facts("./my-save-facts.clp")
CLIPS::Environment.save_facts(env, "./my-save-facts.clp", :local)
env.save_facts("./my-save-facts.clp", :local)
CLIPS::Environment.save_facts(env, "./my-save-facts.clp", :visible)
env.save_facts("./my-save-facts.clp", :visible)
```

### `CLIPS::Environment.load_facts`
### `env.load_facts`

Pass the path to a file that will load the facts stored in the file
into the environment.

```ruby
CLIPS::Environment.load_facts(env, "./my-load-facts.clp")
env.load_facts("./my-load-facts.clp")
```

### `CLIPS::Environment.bsave`
### `env.bsave`

Pass the path in which we'll save the binary representation of the environment constructs.

```ruby
CLIPS::Environment.bsave(env, "./my-bsave.bin")
env.bsave("./my-bsave.bin")
```

### `CLIPS::Environment.bload`
### `env.bload`

Pass the path to a binary file that will load the constructs stored in the file
into the environment. Remember to `reset` the environment if you need.

```ruby
CLIPS::Environment.bload(env, "./my-bload.bin")
env.bload("./my-bload.bin")
```

### `CLIPS::Environment.bsave_facts`
### `env.bsave_facts`

Pass the path in which we'll save the binary representation of the environment facts.
The third argument is optional, and determines whether to store the visible or local facts.
By default, it'll store local.

```ruby
CLIPS::Environment.bsave_facts(env, "./my-bsave-facts.bin")
env.bsave_facts("./my-bsave-facts.bin")
CLIPS::Environment.bsave_facts(env, "./my-bsave-facts.bin", :local)
env.bsave_facts("./my-bsave-facts.bin", :local)
CLIPS::Environment.bsave_facts(env, "./my-bsave-facts.bin", :visible)
env.bsave_facts("./my-bsave-facts.bin", :visible)
```

### `CLIPS::Environment.bload_facts`
### `env.bload_facts`

Pass the path to a binary file that will load the facts stored in the file
into the environment.

```ruby
CLIPS::Environment.bload_facts(env, "./my-bload-facts.bin")
env.bload_facts("./my-bload-facts.bin")
```

### `CLIPS::Environment.assert_string`
### `CLIPS::Environment#assert_string`

Assert a string as a Fact in the CLIPS environment.

```ruby
fact = CLIPS::Environment.assert_string(env, "(foo bar)")
fact2 = env.assert_string("(bat baz)")
```

### `CLIPS::Environment.assert_hash`
### `CLIPS::Environment#assert_hash`

Asserts a Deftemplate fact into the CLIPS environment

```ruby
fact = CLIPS::Environment.assert_hash(env, :my_deftemplate, a: 1, b: "asdf", c: :C)
fact2 = env.assert_hash(:my_deftemplate, d: 4.5, e: :asdf)
```

### `CLIPS::Environment.find_fact`
### `CLIPS::Environment#find_fact`

A light wrapper around the CLIPS find-fact function. Accepts a fact set template and query,
returns the first Facts in the environment that match as Ruby objects.

```ruby
CLIPS::Environment.find_facts(env, "(?f my_deftemplate)")
env.find_fact("(?f my_deftemplate)")
CLIPS::Environment.find_fact(env, "(?f my_deftemplate)", "(eq ?f:b \"asdf\")")
env.find_fact("(?f my_deftemplate)", "(= ?f:a 1)")
```

### `CLIPS::Environment.find_all_facts`
### `CLIPS::Environment#find_all_facts`

A light wrapper around the CLIPS find-all-facts function. Accepts a fact set template and query,
returns Facts in the environment that match as Ruby objects.

```ruby
CLIPS::Environment.find_all_facts(env, "(?f my_deftemplate)")
env.find_all_facts("(?f my_deftemplate)")
CLIPS::Environment.find_all_facts(env, "(?f my_deftemplate)", "(eq ?f:b \"asdf\")")
env.find_all_facts("(?f my_deftemplate)", "(= ?f:a 1)")
```

### `CLIPS::Environment._eval`
### `CLIPS::Environment#_eval`

Evaluates a passed string in the CLIPS environment and returns the results.

```ruby
CLIPS::Environment._eval(env, "(find-all-facts ((?f my_deftemplate)) TRUE)")
env._eval("(find-all-facts ((?f my_deftemplate)) TRUE)")
```

### `CLIPS::Environment.build`
### `CLIPS::Environment#build`

Build the constructs in the env as defined in a string

```ruby
CLIPS::Environment.build(env, "(deftemplate my_template (slot a) (slot b) (slot c))")
env.build("(defrule do-a-thing (my_template (a ?a)) => (println \"a: \" ?a))")
```

### `CLIPS::Environment.add_udf`
### `CLIPS::Environment#add_udf`

Adds a ruby method as a User Defined Function (udf) to the CLIPS environment

```ruby
class CLIPS::Environment
	def foo(a, b=2)
		a + b
	end
	
	def bar(word)
		"You said #{word}!"
	end
end

env.add_udf(:foo)
CLIPS::Environment.add_udf(env, :bar)
```

### `CLIPS::Environment.run`
### `CLIPS::Environment#run`

Runs the rules engine, executing items on the agenda.
Optionally may take the number of items to run from the agenda as an argument.

```ruby
CLIPS::Environment.run(env, 1)
CLIPS::Environment.run(env)
env.run(1)
env.run
```

### `CLIPS::Environment.clear`
### `CLIPS::Environment#clear`

Removes constructs and data from the environment.

```ruby
CLIPS::Environment.clear(env)
env.clear
```

### `CLIPS::Environment.reset`
### `CLIPS::Environment#reset`

Removes all facts and instances.
Creates facts and instances defined in deffacts and definstances constructs.
Resets the values of global variables in the specified environment.

```ruby
CLIPS::Environment.reset(env)
env.reset
```

### `CLIPS::Environment.facts`
### `CLIPS::Environment#facts`

Print all Facts defined in the CLIPS environment

```ruby
CLIPS::Environment.facts(env)
env.facts
```

### `CLIPS::Environment::Fact.to_h`
### `CLIPS::Environment::Fact#to_h`

Returns a hash representing the fact slot names and their values

```ruby
CLIPS::Environment::Fact.to_h(fact)
fact.to_h
```

### `CLIPS::Environment::Fact.index`
### `CLIPS::Environment::Fact#index`

Returns the index of the fact in working memory

```ruby
CLIPS::Environment::Fact.index(fact)
fact.index
```

### `CLIPS::Environment::Fact.deftemplate_name`
### `CLIPS::Environment::Fact#deftemplate_name`

Returns the name of the Deftemplate for a fact as a symbol

```ruby
CLIPS::Environment::Fact.deftemplate_name(fact)
fact.deftemplate_name
```

### `CLIPS::Environment::Fact.slot_names`
### `CLIPS::Environment::Fact#slot_names`

Returns an array representing the slot names of a fact

```ruby
CLIPS::Environment::Fact.slot_names(fact)
fact.slot_names
```

### `CLIPS::Environment::Fact.get_slot`
### `CLIPS::Environment::Fact#get_slot`

Gets the value stored in the specified slot of a fact.
Can either pass a string or symbol for the slot name argument.

```ruby
CLIPS::Environment::Fact.get_slot(fact, 'bar')
fact.get_slot(:foo)
```

### `CLIPS::Environment::Fact.retract`
### `CLIPS::Environment::Fact#retract`

Retracts a fact from the environment.

```ruby
CLIPS::Environment::Fact.retract(fact)
fact.retract
```

### `CLIPS::Environment::Fact.modify`
### `CLIPS::Environment::Fact#modify`

Modifies a non-implicit deftemplate fact from the environment.
Will not work with non-deftemplate facts!

```ruby
CLIPS::Environment::Fact.modify(fact, bar: 123)
fact.modify(foo: "my new foo!")
```

### `CLIPS::Environment.find_defrule`
### `CLIPS::Environment#find_defrule`

Finds a defrule by name and returns a CLIPS::Environment::Defrule object

```ruby
CLIPS::Environment.find_defrule(:a)
env.find_defrule("foo")
```

### `CLIPS::Environment::Defrule.name`
### `CLIPS::Environment::Defrule#name`

Returns the name of a defrule as a symbol

```ruby
CLIPS::Environment::Defrule.name(defrule)
defrule.name
```

### `CLIPS::Environment::Defrule.is_deletable`
### `CLIPS::Environment::Defrule#is_deletable`

Returns a boolean whether the Defrule is deletable or not

```ruby
CLIPS::Environment::Defrule.is_deletable(defrule)
defrule.is_deletable
```

### `CLIPS::Environment::Defrule.pp_form`
### `CLIPS::Environment::Defrule#pp_form`

Returns a pretty printed string representation of the Defrule

```ruby
CLIPS::Environment::Defrule.pp_form(defrule)
defrule.pp_form
```

### `CLIPS::Environment::Defrule.has_breakpoint`
### `CLIPS::Environment::Defrule#has_breakpoint`

Returns whether or not the rule has a breakpoint set

```ruby
CLIPS::Environment::Defrule.has_breakpoint(defrule)
defrule.has_breakpoint
```

### `CLIPS::Environment::Defrule.set_break`
### `CLIPS::Environment::Defrule#set_break`

Sets a breakpoint on a rule

```ruby
CLIPS::Environment::Defrule.set_break(defrule)
defrule.set_break
```

### `CLIPS::Environment::Defrule.remove_break`
### `CLIPS::Environment::Defrule#remove_break`

Returns whether or not it was able to remove a breakpoint from a rule

```ruby
CLIPS::Environment::Defrule.remove_break(defrule)
defrule.remove_break
```

## Running the tests

Simply do `rake compile` and then `rake test` in order to run the tests.
