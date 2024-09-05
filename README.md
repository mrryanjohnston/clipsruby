# clipsruby

## Description

Use the CLIPS programming language from within Ruby

## API

### `CLIPS.create_environment`
### `CLIPS::Environment.new`

Create a new CLIPS environment in which you may define Rules,
assert Facts, and instantiate Objects.

```ruby
env = CLIPS.create_environment
env2 = CLIPS::Environment.new
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

### `CLIPS::Environment.facts`
### `CLIPS::Environment#facts`

Print all Facts defined in the CLIPS environment

```ruby
CLIPS::Environment.facts(env)
env.facts
```

### `CLIPS::Environment::Fact.deftemplate_name`
### `CLIPS::Environment::Fact#deftemplate_name`

Returns the name of the Deftemplate for a fact as a symbol

```ruby
CLIPS::Environment::Fact.deftemplate_name(fact)
fact.deftemplate_name
```
