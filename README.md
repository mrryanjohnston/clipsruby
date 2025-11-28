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

### Environment Methods

#### `CLIPS.create_environment` / `CLIPS::Environment.new`

Create a new CLIPS environment in which you may define Rules,
assert Facts, and instantiate Objects.

```ruby
env = CLIPS.create_environment
env2 = CLIPS::Environment.new
```

#### `CLIPS::Environment._eval` / `CLIPS::Environment#_eval`

Evaluates a passed string in the CLIPS environment and returns the results.

```ruby
CLIPS::Environment._eval(env, "(find-all-facts ((?f my_deftemplate)) TRUE)")
env._eval("(find-all-facts ((?f my_deftemplate)) TRUE)")
```

#### `CLIPS::Environment.build` / `CLIPS::Environment#build`

Build the constructs in the env as defined in a string

```ruby
CLIPS::Environment.build(env, "(deftemplate my_template (slot a) (slot b) (slot c))")
env.build("(defrule do-a-thing (my_template (a ?a)) => (println \"a: \" ?a))")
```

#### `CLIPS::Environment.add_udf` / `CLIPS::Environment#add_udf`

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

#### `CLIPS::Environment.run` / `CLIPS::Environment#run`

Runs the rules engine, executing items on the agenda.
Optionally may take the number of items to run from the agenda as an argument.

```ruby
CLIPS::Environment.run(env, 1)
CLIPS::Environment.run(env)
env.run(1)
env.run
```

#### `CLIPS::Environment.clear` / `CLIPS::Environment#clear`

Removes constructs and data from the environment.

```ruby
CLIPS::Environment.clear(env)
env.clear
```

#### `CLIPS::Environment.reset` / `CLIPS::Environment#reset`

Removes all facts and instances.
Creates facts and instances defined in deffacts and definstances constructs.
Resets the values of global variables in the specified environment.

```ruby
CLIPS::Environment.reset(env)
env.reset
```

#### `CLIPS::Environment.batch_star` / `env.batch_star`

Pass the path to a file that will be "batched" into the environment,
allowing many CLIPS commands to be entered at once.

```ruby
CLIPS::Environment.batch_star(env, "./my-batch.bat")
env.batch_star("./my-batch.bat")
```

#### `CLIPS::Environment.save` / `env.save`

Pass the path in which we'll save the environment constructs.

```ruby
CLIPS::Environment.save(env, "./my-save.clp")
env.save("./my-save.clp")
```

#### `CLIPS::Environment.load` / `env.load`

Pass the path to a file that will load the constructs stored in the file
into the environment. Remember to `reset` the environment if you need.

```ruby
CLIPS::Environment.load(env, "./my-load.clp")
env.load("./my-load.clp")
```

#### `CLIPS::Environment.save_facts` / `env.save_facts`

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

#### `CLIPS::Environment.load_facts` / `env.load_facts`

Pass the path to a file that will load the facts stored in the file
into the environment.

```ruby
CLIPS::Environment.load_facts(env, "./my-load-facts.clp")
env.load_facts("./my-load-facts.clp")
```

#### `CLIPS::Environment.save_instances` / `env.save_instances`

Pass the path in which we'll save the environment instances.
The third argument is optional, and determines whether to store the visible or local instances.
By default, it'll store local.

```ruby
CLIPS::Environment.save_instances(env, "./my-save-instances.clp")
env.save_instances("./my-save-instances.clp")
CLIPS::Environment.save_instances(env, "./my-save-instances.clp", :local)
env.save_instances("./my-save-instances.clp", :local)
CLIPS::Environment.save_instances(env, "./my-save-instances.clp", :visible)
env.save_instances("./my-save-instances.clp", :visible)
```

#### `CLIPS::Environment.load_instances` / `env.load_instances`

Pass the path to a file that will load the instances stored in the file
into the environment.

```ruby
CLIPS::Environment.load_instances(env, "./my-load-instances.clp")
env.load_instances("./my-load-instances.clp")
```

#### `CLIPS::Environment.bsave` / `env.bsave`

Pass the path in which we'll save the binary representation of the environment constructs.

```ruby
CLIPS::Environment.bsave(env, "./my-bsave.bin")
env.bsave("./my-bsave.bin")
```

#### `CLIPS::Environment.bload` / `env.bload`

Pass the path to a binary file that will load the constructs stored in the file
into the environment. Remember to `reset` the environment if you need.

```ruby
CLIPS::Environment.bload(env, "./my-bload.bin")
env.bload("./my-bload.bin")
```

#### `CLIPS::Environment.bsave_facts` / `env.bsave_facts`

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

#### `CLIPS::Environment.bload_facts` / `env.bload_facts`

Pass the path to a binary file that will load the facts stored in the file
into the environment.

```ruby
CLIPS::Environment.bload_facts(env, "./my-bload-facts.bin")
env.bload_facts("./my-bload-facts.bin")
```

#### `CLIPS::Environment.bsave_instances` / `env.bsave_instances`

Pass the path in which we'll save the binary representation of the environment instances.
The third argument is optional, and determines whether to store the visible or local instances.
By default, it'll store local.

```ruby
CLIPS::Environment.bsave_instances(env, "./my-bsave-instances.bin")
env.bsave_instances("./my-bsave-instances.bin")
CLIPS::Environment.bsave_instances(env, "./my-bsave-instances.bin", :local)
env.bsave_instances("./my-bsave-instances.bin", :local)
CLIPS::Environment.bsave_instances(env, "./my-bsave-instances.bin", :visible)
env.bsave_instances("./my-bsave-instances.bin", :visible)
```

#### `CLIPS::Environment.bload_facts` / `env.bload_facts`

Pass the path to a binary file that will load the facts stored in the file
into the environment.

```ruby
CLIPS::Environment.bload_facts(env, "./my-bload-facts.bin")
env.bload_facts("./my-bload-facts.bin")
```


#### `CLIPS::Environment.assert_string` / `CLIPS::Environment#assert_string`

Assert a string as a Fact in the CLIPS environment.

```ruby
fact = CLIPS::Environment.assert_string(env, "(foo bar)")
fact2 = env.assert_string("(bat baz)")
```

#### `CLIPS::Environment.assert_hash` / `CLIPS::Environment#assert_hash`

Asserts a Deftemplate fact into the CLIPS environment

```ruby
fact = CLIPS::Environment.assert_hash(env, :my_deftemplate, a: 1, b: "asdf", c: :C)
fact2 = env.assert_hash(:my_deftemplate, d: 4.5, e: :asdf)
```

#### `CLIPS::Environment.make_instance` / `CLIPS::Environment#make_instance`

Make an Instance in the CLIPS environment.

The first argument can be:

* a symbol/string representing a string you'd call `make-instance` with in CLIPS code
* a symbol/string representing a Defclass in the environment
* a `CLIPS::Environment::Defclass` object

The second argument can be:

* a symbol/string representing the name of the instance you'd like
* a hash holding the key/value pairs for instance slot assignment
* `nil`

The third argument can be:

* a hash holding the key/value pairs for instance slot assignment

```ruby
instance = CLIPS::Environment.make_instance(env, "(of USER)")
instance2 = env.make_instance("(of USER)")
instance3 = CLIPS::Environment.make_instance(env, :USER)
instance4 = env.make_instance(:USER)
instance5 = CLIPS::Environment.make_instance(env, env.find_defclass(:USER))
instance6 = env.make_instance(env.find_defclass(:USER))
instance7 = CLIPS::Environment.make_instance(env, env.find_defclass(:my_class), foo: "Bar!")
instance8 = env.make_instance(env.find_defclass(:my_class), foo: "Bar!")
instance9 = CLIPS::Environment.make_instance(env, env.find_defclass(:my_class), :my_instance1, foo: "Bar!")
instance10 = env.make_instance(env.find_defclass(:my_class), :my_instance2, foo: "Bar.")
instance11 = CLIPS::Environment.make_instance(env, env.find_defclass(:my_class), nil, foo: "Bar?")
instance12 = env.make_instance(env.find_defclass(:my_class), nil, foo: "Bar...")
```

#### `CLIPS::Environment.find_fact` / `CLIPS::Environment#find_fact`

A light wrapper around the CLIPS find-fact function. Accepts a fact set template and query,
returns the first Facts in the environment that match as Ruby objects.

```ruby
CLIPS::Environment.find_fact(env, "(?f my_deftemplate)")
env.find_fact("(?f my_deftemplate)")
CLIPS::Environment.find_fact(env, "(?f my_deftemplate)", "(eq ?f:b \"asdf\")")
env.find_fact("(?f my_deftemplate)", "(= ?f:a 1)")
```

#### `CLIPS::Environment.get_fact_list` / `CLIPS::Environment#get_fact_list`

Return an array of Facts in the environment. Pass an argument of a
symbol, string, or Defmodule object in order to only get Facts with deftemplates
in that Defmodule. If you do not, it will return all Facts in all modules.

```ruby
CLIPS::Environment.get_fact_list
env.get_fact_list
CLIPS::Environment.get_fact_list(:MAIN)
env.get_fact_list(:MAIN)
CLIPS::Environment.get_fact_list(defmodule)
env.get_fact_list(defmodule)
```

#### `CLIPS::Environment.find_all_facts` / `CLIPS::Environment#find_all_facts`

A light wrapper around the CLIPS find-all-facts function. Accepts a fact set template and query,
returns Facts in the environment that match as Ruby objects.

```ruby
CLIPS::Environment.find_all_facts(env, "(?f my_deftemplate)")
env.find_all_facts("(?f my_deftemplate)")
CLIPS::Environment.find_all_facts(env, "(?f my_deftemplate)", "(eq ?f:b \"asdf\")")
env.find_all_facts("(?f my_deftemplate)", "(= ?f:a 1)")
```

#### `CLIPS::Environment.facts` / `CLIPS::Environment#facts`

Print all Facts defined in the CLIPS environment

```ruby
CLIPS::Environment.facts(env)
env.facts
```

#### `CLIPS::Environment.get_current_module` / `CLIPS::Environment#get_current_module`

Get the current module in the CLIPS environment

```ruby
CLIPS::Environment.get_current_module(env)
env.get_current_module
```

#### `CLIPS::Environment.set_current_module` / `CLIPS::Environment#set_current_module`

Set the current module in the CLIPS environment

```ruby
CLIPS::Environment.set_current_module(env, defmodule) 
env.set_current_module(defmodule)
```

#### `CLIPS::Environment.find_defrule` / `CLIPS::Environment#find_defrule`

Finds a defrule by name and returns a CLIPS::Environment::Defrule object

```ruby
CLIPS::Environment.find_defrule(:a)
env.find_defrule("foo")
```

#### `CLIPS::Environment.find_instance` / `CLIPS::Environment#find_instance`

Finds an instance by name and returns a CLIPS::Environment::Instance object.
Can take an optional second and third argument for Defmodule or Demodule name
and a boolean that represents whether or not the instance should be searched for
in imported modules.
If the second argument is `nil`, `find_instance` will search the current module only.

```ruby
CLIPS::Environment.find_definstance(:a)
env.find_definstance("foo")
CLIPS::Environment.find_definstance(:a, :my_module)
env.find_definstance("foo", "my_module")
CLIPS::Environment.find_definstance(:a, :my_module, true)
env.find_definstance("foo", nil, true)
```

#### `CLIPS::Environment.find_defmodule` / `CLIPS::Environment#find_defmodule`

Finds a defmodule by name and returns a CLIPS::Environment::Defmodule object

```ruby
CLIPS::Environment.find_defmodule(:a)
env.find_defmodule("foo")
```

#### `CLIPS::Environment.get_activation_list` / `CLIPS::Environment#get_activation_list`

Return an array of Activation objects in the current demodule.

```ruby
CLIPS::Environment.get_activation_list
env.get_activation_list
```

#### `CLIPS::Environment.get_defclass_list` / `CLIPS::Environment#get_defclass_list`

Return an array of Defclass names as symbols in the environment. Pass an argument of a
symbol, string, or Defmodule object in order to only get Defclasss
in that Defmodule. If you do not, it will return all Defclass names in all modules.

```ruby
CLIPS::Environment.get_defclass_list(env)
env.get_defclass_list
CLIPS::Environment.get_defclass_list(env, :MAIN)
env.get_defclass_list(:MAIN)
CLIPS::Environment.get_defclass_list(env, defmodule)
env.get_defclass_list(defmodule)
```

#### `CLIPS::Environment.get_instance_list` / `CLIPS::Environment#get_instance_list`

Return an array of Instances in the environment. Pass an argument of a
symbol, string, or Defclass object in order to only get Instances
in that Defclass. If you do not, it will return all Instances of all Defclasses.
If you do, you may also pass `true` as the second argument
to include instances of subclasses of this Defclass.

```ruby
CLIPS::Environment.get_instance_list(env)
env.get_instance_list
CLIPS::Environment.get_instance_list(env, :foo_class)
env.get_instance_list(:"my-class")
CLIPS::Environment.get_instance_list(env, defclass)
env.get_instance_list(defclass)
CLIPS::Environment.get_instance_list(env, defclass, true)
env.get_instance_list(defclass, true)
```

#### `CLIPS::Environment.get_deftemplate_list` / `CLIPS::Environment#get_deftemplate_list`

Return an array of Deftemplate names as symbols in the environment. Pass an argument of a
symbol, string, or Defmodule object in order to only get Deftemplates
in that Defmodule. If you do not, it will return all Deftemplate names in all modules.

```ruby
CLIPS::Environment.get_deftemplate_list(env)
env.get_deftemplate_list
CLIPS::Environment.get_deftemplate_list(env, :MAIN)
env.get_deftemplate_list(:MAIN)
CLIPS::Environment.get_deftemplate_list(env, defmodule)
env.get_deftemplate_list(defmodule)
```

#### `CLIPS::Environment.get_defrule_list` / `CLIPS::Environment#get_defrule_list`

Return an array of Defrule names as symbols in the environment. Pass an argument of a
symbol, string, or Defmodule object in order to only get Defrules
in that Defmodule. If you do not, it will return all Defrule names in all modules.

```ruby
CLIPS::Environment.get_defrule_list(env)
env.get_defrule_list
CLIPS::Environment.get_defrule_list(env, :MAIN)
env.get_defrule_list(:MAIN)
CLIPS::Environment.get_defrule_list(env, defmodule)
env.get_defrule_list(defmodule)
```

#### `CLIPS::Environment.get_defmodule_list` / `CLIPS::Environment#get_defmodule_list`

Return an array of Defmodule names as symbols in the environment.

```ruby
CLIPS::Environment.get_defmodule_list(env)
env.get_defmodule_list
```

#### `CLIPS::Environment.get_deffunction_list` / `CLIPS::Environment#get_deffunction_list`

Return an array of Deffunction names as symbols in the environment.
Optionally pass a `Defmodule`.

```ruby
CLIPS::Environment.get_deffunction_list(env)
env.get_deffunction_list
CLIPS::Environment.get_deffunction_list(env, :MAIN)
env.get_deffunction_list(defmodule)
```

#### `CLIPS::Environment.find_deftemplate` / `CLIPS::Environment#find_deftemplate`

Finds a deftemplate by name and returns a CLIPS::Environment::Deftemplate object

```ruby
CLIPS::Environment.find_deftemplate(:a)
env.find_deftemplate("foo")
```

#### `CLIPS::Environment.find_defclass` / `CLIPS::Environment#find_defclass`

Finds a defclass by name and returns a CLIPS::Environment::Defclass object

```ruby
CLIPS::Environment.find_defclass(:a)
env.find_defclass("foo")
```

#### `CLIPS::Environment.watch` / `env.watch`
#### `CLIPS::Environment.unwatch` / `env.unwatch`

"Watch" or "Unwatch" a specific thing that happens in the CLIPS environment.
There are several things that may be watched (or unwatched) such that CLIPS will
report debugging information to `STDOUT`. This gem provides two ways to watch
or unwatch a debug item: either pass the watch item as a symbol to `watch` or
`unwatch`, or use the corresponding `watch_foo` or `unwatch_foo` methods where
`foo` is replaced by the watch item (as listed):

```ruby
:all
:facts
:instances
:slots
:rules
:activations
:messages
:message_handlers
:generic_functions
:methods
:deffunctions
:compilations
:statistics
:globals
:focus
```

```ruby
CLIPS::Environment.watch_facts
env.watch_statistics
CLIPS::Environment.unwatch(:facts)
env.unwatch(:statistics)
```

#### `CLIPS::Environment#refresh_all_agendas` / `CLIPS::Environment.refresh_all_agendas`

Refresh agendas in all `Defmodule`s in the CLIPS `Environment`.

```ruby
CLIPS::Environment.refresh_all_agendas(env)
env.refresh_all_agendas
```

#### `CLIPS::Environment#reorder_all_agendas` / `CLIPS::Environment.reorder_all_agendas`

Reorder agendas in all `Defmodule`s in the CLIPS `Environment`.

```ruby
CLIPS::Environment.reorder_all_agendas(env)
env.reorder_all_agendas
```

#### `CLIPS::Environment#get_agenda_changed` / `CLIPS::Environment.get_agenda_changed`

Returns whether or not the agenda was changed in the `Environment`.

```ruby
CLIPS::Environment.get_agenda_changed(env)
env.get_agenda_changed
```

#### `CLIPS::Environment#set_agenda_changed` / `CLIPS::Environment.set_agenda_changed`

Sets whether or not the agenda was changed in the `Environment`.

```ruby
CLIPS::Environment.set_agenda_changed(env, false)
env.set_agenda_changed true
```

#### `CLIPS::Environment#get_salience_evaluation` / `CLIPS::Environment.get_salience_evaluation`

Returns the salience evaluation current set in the `Environment`.
Possible return values are:

- `:"when-defined"`
- `:"when-activated"`
- `:"every-cycle"`
- `:unknown`

```ruby
CLIPS::Environment.get_salience_evaluation(env)
env.get_salience_evaluation
```

#### `CLIPS::Environment#set_salience_evaluation` / `CLIPS::Environment.set_salience_evaluation`

Sets the salience evaluation in the `Environment`.
Possible arguments are:

- `:"when-defined"`
- `:"when-activated"`
- `:"every-cycle"`
- `:unknown`

```ruby
CLIPS::Environment.set_salience_evaluation(env, :"when-defined")
env.set_salience_evaluation :"every-cycle"
```

#### `CLIPS::Environment#get_strategy` / `CLIPS::Environment.get_strategy`

Returns the strategy current set in the `Environment`.
Possible return values are:

- `:depth`
- `:breadth`
- `:lex`
- `:mea`
- `:complexity`
- `:simplicity`
- `:random`
- `:unknown`

```ruby
CLIPS::Environment.get_strategy(env)
env.get_strategy
```

#### `CLIPS::Environment#set_strategy` / `CLIPS::Environment.set_strategy`

Sets the strategy in the `Environment`.
Possible arguments are:

- `:depth`
- `:breadth`
- `:lex`
- `:mea`
- `:complexity`
- `:simplicity`
- `:random`
- `:unknown`

```ruby
CLIPS::Environment.set_strategy(env, :lex)
env.set_strategy :mea
```

### Fact Methods

#### `CLIPS::Environment::Fact.pp_form` / `CLIPS::Environment::Fact#pp_form`

Returns a pretty printed string representation of the Fact

```ruby
CLIPS::Environment::Fact.pp_form(fact)
fact.pp_form
```

#### `CLIPS::Environment::Fact.existp` / `CLIPS::Environment::Fact#existp`

Returns a boolean representing whether or not a Fact has been retracted

```ruby
CLIPS::Environment::Fact.existp(fact)
fact.existp
```

#### `CLIPS::Environment::Fact.to_h` / `CLIPS::Environment::Fact#to_h`

Returns a hash representing the fact slot names and their values

```ruby
CLIPS::Environment::Fact.to_h(fact)
fact.to_h
```

#### `CLIPS::Environment::Fact.index` / `CLIPS::Environment::Fact#index`

Returns the index of the fact in working memory

```ruby
CLIPS::Environment::Fact.index(fact)
fact.index
```

#### `CLIPS::Environment::Fact.deftemplate` / `CLIPS::Environment::Fact#deftemplate`

Returns the Deftemplate object for a fact

```ruby
CLIPS::Environment::Fact.deftemplate(fact)
fact.deftemplate
```

#### `CLIPS::Environment::Fact.deftemplate_name` / `CLIPS::Environment::Fact#deftemplate_name`

Returns the name of the Deftemplate for a fact as a symbol

```ruby
CLIPS::Environment::Fact.deftemplate_name(fact)
fact.deftemplate_name
```

#### `CLIPS::Environment::Fact.slot_names` / `CLIPS::Environment::Fact#slot_names`

Returns an array representing the slot names of a fact

```ruby
CLIPS::Environment::Fact.slot_names(fact)
fact.slot_names
```

#### `CLIPS::Environment::Fact.get_slot` / `CLIPS::Environment::Fact#get_slot`

Gets the value stored in the specified slot of a fact.
Can either pass a string or symbol for the slot name argument.

```ruby
CLIPS::Environment::Fact.get_slot(fact, 'bar')
fact.get_slot(:foo)
```

#### `CLIPS::Environment::Fact.retract` / `CLIPS::Environment::Fact#retract`

Retracts a fact from the environment.

```ruby
CLIPS::Environment::Fact.retract(fact)
fact.retract
```

#### `CLIPS::Environment::Fact.modify` / `CLIPS::Environment::Fact#modify`

Modifies a non-implicit deftemplate fact from the environment.
Will not work with non-deftemplate facts!

```ruby
CLIPS::Environment::Fact.modify(fact, bar: 123)
fact.modify(foo: "my new foo!")
```

### Instance Methods

#### `CLIPS::Environment::Instance.unmake` / `CLIPS::Environment::Instance#unmake`

Unmakes an instance in the environment, deleting it.

```ruby
CLIPS::Environment::Instance.unmake(instance)
instance.unmake
```

#### `CLIPS::Environment::Instance.delete` / `CLIPS::Environment::Instance#delete`

Deletes an instance in the environment.

```ruby
CLIPS::Environment::Instance.delete(instance)
instance.delete
```

#### `CLIPS::Environment::Instance.name` / `CLIPS::Environment::Instance#name`

Returns the name of an Instance as a symbol

```ruby
CLIPS::Environment::Instance.name(instance)
instance.name
```

#### `CLIPS::Environment::Instance.pp_form` / `CLIPS::Environment::Instance#pp_form`

Returns a pretty printed string representation of an Instance

```ruby
CLIPS::Environment::Instance.pp_form(instance)
instance.pp_form
```

#### `CLIPS::Environment::Instance.defclass` / `CLIPS::Environment::Instance#defclass`

Returns the Defclass of an Instance

```ruby
CLIPS::Environment::Instance.defclass(instance)
instance.defclass
```

#### `CLIPS::Environment::Instance.to_h` / `CLIPS::Environment::Instance#to_h`

Returns a hash representing the instance slot names and their values.
Pass a `true` as an argument to get inhereted slots as well.

```ruby
CLIPS::Environment::Instance.to_h(instance)
instance.to_h
CLIPS::Environment::Instance.to_h(instance, true)
instance.to_h(true)
```

### Defclass Methods

#### `CLIPS::Environment::Defclass.name` / `CLIPS::Environment::Defclass#name`

Returns the name of an Defclass as a symbol

```ruby
CLIPS::Environment::Defclass.name(defclass)
defclass.name
```

#### `CLIPS::Environment::Defclass.pp_form` / `CLIPS::Environment::Defclass#pp_form`

Returns a pretty printed string representation of an Defclass

```ruby
CLIPS::Environment::Defclass.pp_form(defclass)
defclass.pp_form
```

#### `CLIPS::Environment::Defclass.slots` / `CLIPS::Environment::Defclass#slots`

Returns an array representing the slot names of a Defclass.
Pass `true` as an argument to get inherited slots, as well.

```ruby
CLIPS::Environment::Defclass.slots(defclass)
defclass.slots
CLIPS::Environment::Defclass.slots(defclass, true)
defclass.slots(true)
```

#### `CLIPS::Environment::Defclass.defmodule_name` / `CLIPS::Environment::Defclass#defmodule_name`

Returns the name of the defmodule in which the defclass is defined

```ruby
CLIPS::Environment::Defclass.defmodule_name(defclass)
defclass.defmodule_name
```

#### `CLIPS::Environment::Defclass.defmodule` / `CLIPS::Environment::Defclass#defmodule`

Returns the defmodule in which the defclass is defined

```ruby
CLIPS::Environment::Defclass.defmodule(defclass)
defclass.defmodule
```

#### `CLIPS::Environment::Defclass.get_instance_list` / `CLIPS::Environment::Defclass#get_instance_list`

Return an array of Instances of the Defclass.
Pass `true` as an argument to include instances of subclasses
of this Defclass.

```ruby
CLIPS::Environment::Defclass.get_instance_list(defclass)
CLIPS::Environment::Defclass.get_instance_list(defclass, true)
defclass.get_instance_list
defclass.get_instance_list(true)
```

#### `CLIPS::Environment::Defclass.superclasses` / `CLIPS::Environment::Defclass#superclasses`

Return an array of superclasses of the Defclass.
Pass `true` as an argument to include inherited superclasses.

```ruby
CLIPS::Environment::Defclass.superclasses(defclass)
CLIPS::Environment::Defclass.superclasses(defclass, true)
defclass.superclasses
defclass.superclasses(true)
```

#### `CLIPS::Environment::Defclass.subclasses` / `CLIPS::Environment::Defclass#subclasses`

Return an array of subclasses of the Defclass.
Pass `true` as an argument to include inherited subclasses.

```ruby
CLIPS::Environment::Defclass.subclasses(defclass)
CLIPS::Environment::Defclass.subclasses(defclass, true)
defclass.subclasses
defclass.subclasses(true)
```

### Defrule Methods

#### `CLIPS::Environment::Defrule.name` / `CLIPS::Environment::Defrule#name`

Returns the name of a defrule as a symbol

```ruby
CLIPS::Environment::Defrule.name(defrule)
defrule.name
```

#### `CLIPS::Environment::Defrule.defmodule_name` / `CLIPS::Environment::Defrule#defmodule_name`

Returns the name of the defmodule in which the defrule is defined

```ruby
CLIPS::Environment::Defrule.defmodule_name(defrule)
defrule.defmodule_name
```

#### `CLIPS::Environment::Defrule.defmodule` / `CLIPS::Environment::Defrule#defmodule`

Returns the defmodule in which the defrule is defined

```ruby
CLIPS::Environment::Defrule.defmodule(defrule)
defrule.defmodule
```

#### `CLIPS::Environment::Defrule.is_deletable` / `CLIPS::Environment::Defrule#is_deletable`

Returns a boolean whether the Defrule is deletable or not

```ruby
CLIPS::Environment::Defrule.is_deletable(defrule)
defrule.is_deletable
```

#### `CLIPS::Environment::Defrule.pp_form` / `CLIPS::Environment::Defrule#pp_form`

Returns a pretty printed string representation of the Defrule

```ruby
CLIPS::Environment::Defrule.pp_form(defrule)
defrule.pp_form
```

#### `CLIPS::Environment::Defrule.has_breakpoint` / `CLIPS::Environment::Defrule#has_breakpoint`

Returns whether or not the rule has a breakpoint set

```ruby
CLIPS::Environment::Defrule.has_breakpoint(defrule)
defrule.has_breakpoint
```

#### `CLIPS::Environment::Defrule.set_break` / `CLIPS::Environment::Defrule#set_break`

Sets a breakpoint on a rule

```ruby
CLIPS::Environment::Defrule.set_break(defrule)
defrule.set_break
```

#### `CLIPS::Environment::Defrule.remove_break` / `CLIPS::Environment::Defrule#remove_break`

Returns whether or not it was able to remove a breakpoint from a rule

```ruby
CLIPS::Environment::Defrule.remove_break(defrule)
defrule.remove_break
```

#### `CLIPS::Environment::Defrule.salience` / `CLIPS::Environment::Defrule#salience`

Returns the salience of a defrule

```ruby
CLIPS::Environment::Defrule.salience(defrule)
defrule.salience
```

#### `CLIPS::Environment::Defrule.matches` / `CLIPS::Environment::Defrule#matches`

Returns an array holding the number of pattern matches, partial matches,
and activations for a given rule. Pass a first argument of
`:verbose`, `:succinct`, or `:terse` to determine the amount of information
to print to `STDOUT` (`:verbose` by default).

```ruby
CLIPS::Environment::Defrule.matches(defrule, :succinct)
defrule.matches(:terse)
[pattern, partial, activations] = defrule.matches
```

### Defmodule Methods

#### `CLIPS::Environment::Defmodule.name` / `CLIPS::Environment::Defmodule#name`

Returns the name of a defmodule as a symbol

```ruby
CLIPS::Environment::Defmodule.name(defmodule)
defmodule.name
```

#### `CLIPS::Environment::Defmodule.pp_form` / `CLIPS::Environment::Defmodule#pp_form`

Returns a pretty printed string representation of the Defmodule

```ruby
CLIPS::Environment::Defmodule.pp_form(defmodule)
defmodule.pp_form
```

#### `CLIPS::Environment::Defmodule.set_current` / `CLIPS::Environment::Defmodule#set_current`

Sets the Defmodule as the current module of the environment

```ruby
CLIPS::Environment::Defmodule.set_current(defmodule)
defmodule.set_current
```

#### `CLIPS::Environment::Defmodule.get_fact_list` / `CLIPS::Environment::Defmodule#get_fact_list`

Return an array of Facts with deftemplates in the Defmodule

```ruby
CLIPS::Environment::Defmodule.get_fact_list(defmodule)
defmodule.get_fact_list
```

#### `CLIPS::Environment::Defmodule.find_instance` / `CLIPS::Environment::Defmodule#find_instance`

Finds an instance by name and returns a CLIPS::Environment::Instance object.
Can take an optional second argument
of a boolean that represents whether or not the instance should be searched for
in imported modules.

```ruby
CLIPS::Environment::Defmodule.find_instance(:a)
defmodule.find_instance("foo")
CLIPS::Environment::Defmodule.find_instance(:a, true)
defmodule.find_instance("foo", true)
```

#### `CLIPS::Environment::Defmodule.get_defclass_list` / `CLIPS::Environment::Defmodule#get_defclass_list`

Return an array of Defclass names as symbols in the Defmodule

```ruby
CLIPS::Environment::Defmodule.get_defclass_list(defmodule)
defmodule.get_defclass_list
```

#### `CLIPS::Environment::Defmodule.get_deftemplate_list` / `CLIPS::Environment::Defmodule#get_deftemplate_list`

Return an array of Deftemplate names as symbols in the Defmodule

```ruby
CLIPS::Environment::Defmodule.get_deftemplate_list(defmodule)
defmodule.get_deftemplate_list
```

#### `CLIPS::Environment::Defmodule.get_defrule_list` / `CLIPS::Environment::Defmodule#get_defrule_list`

Return an array of Defrule names as symbols in the Defmodule

```ruby
CLIPS::Environment::Defmodule.get_defrule_list(defmodule)
defmodule.get_defrule_list
```

#### `CLIPS::Environment::Defmodule.get_deffunction_list` / `CLIPS::Environment::Defmodule#get_deffunction_list`

Return an array of Deffunction names as symbols in the Defmodule

```ruby
CLIPS::Environment::Defmodule.get_deffunction_list(defmodule)
defmodule.get_deffunction_list
```

#### `CLIPS::Environment::Defmodule.refresh_agenda` / `CLIPS::Environment::Defmodule#refresh_agenda` 

Refreshes the agenda in a given Defmodule.
If you used `CLIPS::Environment::Activation.set_salience` / 
`CLIPS::Environment::Activation#set_salience` to set the salience
of an activation, using `refresh_agenda` will set it back to its
original salience.

```ruby
CLIPS::Environment::Defmodule.refresh_agenda(defmodule)
defmodule.refresh_agenda
```

#### `CLIPS::Environment::Defmodule.reorder_agenda` / `CLIPS::Environment::Defmodule#reorder_agenda` 

Refreshes the agenda in a given Defmodule.
If you used `CLIPS::Environment::Activation.set_salience` / 
`CLIPS::Environment::Activation#set_salience` to set the salience
of an activation, using `reorder_agenda` will re-order the agenda
according to the new salience of activations on the agenda.

```ruby
CLIPS::Environment::Defmodule.reorder_agenda(defmodule)
defmodule.reorder_agenda
```

#### `CLIPS::Environment::Defmodule.delete_all_activations` / `CLIPS::Environment::Defmodule#delete_all_activations` 

Deletes all activations in the agenda in a given Defmodule.

```ruby
CLIPS::Environment::Defmodule.delete_all_activations(defmodule)
defmodule.delete_all_activations
```

### Deftemplate Methods

#### `CLIPS::Environment::Deftemplate.name` / `CLIPS::Environment::Deftemplate#name`

Returns the name of a deftemplate as a symbol

```ruby
CLIPS::Environment::Deftemplate.name(deftemplate)
deftemplate.name
```

#### `CLIPS::Environment::Deftemplate.pp_form` / `CLIPS::Environment::Deftemplate#pp_form`

Returns a pretty printed string representation of the Deftemplate

```ruby
CLIPS::Environment::Deftemplate.pp_form(deftemplate)
deftemplate.pp_form
```

#### `CLIPS::Environment::Deftemplate.assert_hash` / `CLIPS::Environment::Deftemplate#assert_hash`

Asserts a hash in the clips environment that a Deftemplate is defined in.
Returns the Fact object that was just asserted

```ruby
CLIPS::Environment::Deftemplate.assert_hash(deftemplate, foo: :bar)
deftemplate.assert_hash(foo: :bar)
```

#### `CLIPS::Environment::Deftemplate.defmodule_name` / `CLIPS::Environment::Deftemplate#defmodule_name`

Returns the name of the defmodule in which the deftemplate is defined

```ruby
CLIPS::Environment::Deftemplate.defmodule_name(deftemplate)
deftemplate.defmodule_name
```

#### `CLIPS::Environment::Deftemplate.defmodule` / `CLIPS::Environment::Deftemplate#defmodule`

Returns the defmodule in which the deftemplate is defined

```ruby
CLIPS::Environment::Deftemplate.defmodule(deftemplate)
deftemplate.defmodule
```

#### `CLIPS::Environment::Deftemplate.slot_names` / `CLIPS::Environment::Deftemplate#slot_names`

Returns the slot names of a deftemplate as symbols

```ruby
CLIPS::Environment::Deftemplate.slot_names(deftemplate)
deftemplate.slot_names
```

#### `CLIPS::Environment::Deftemplate.is_deletable` / `CLIPS::Environment::Deftemplate#is_deletable`

Returns a boolean whether the Deftemplate is deletable or not

```ruby
CLIPS::Environment::Deftemplate.is_deletable(deftemplate)
deftemplate.is_deletable
```

#### `CLIPS::Environment::Deftemplate.is_implied` / `CLIPS::Environment::Deftemplate#is_implied`

Returns a boolean whether the Deftemplate is implied or not.
If the fact is an ordered fact, the Deftemplate is implied.
Otherwise, the Deftemplate is considered explicitly defined.

```ruby
CLIPS::Environment::Deftemplate.is_implied(deftemplate)
deftemplate.is_implied
```

#### `CLIPS::Environment::Deftemplate.slot_allowed_values` / `CLIPS::Environment::Deftemplate#slot_allowed_values`

Returns the allowed values for the slot of a deftemplate as symbols

```ruby
CLIPS::Environment::Deftemplate.slot_allowed_values(deftemplate, :foo)
deftemplate.slot_allowed_values('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_default_value` / `CLIPS::Environment::Deftemplate#slot_default_value`

Returns the default value(s) for the slot of a deftemplate as symbols

```ruby
CLIPS::Environment::Deftemplate.slot_default_value(deftemplate, :foo)
deftemplate.slot_default_value('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_types` / `CLIPS::Environment::Deftemplate#slot_types`

Returns the slot type(s) for the named slot of a deftemplate as symbols.
Possible types are as follows:

```ruby
:SYMBOL
:STRING
:LEXEME
:INTEGER
:FLOAT
:NUMBER
:"INSTANCE-NAME"
:"INSTANCE-ADDRESS"
:INSTANCE
:"EXTERNAL-ADDRESS"
:"FACT-ADDRESS"
```

```ruby
CLIPS::Environment::Deftemplate.slot_types(deftemplate, :foo)
deftemplate.slot_types('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_range` / `CLIPS::Environment::Deftemplate#slot_range`

Returns the range of a named slot of a deftemplate as integers or symbols (in case of infinity).

```ruby
CLIPS::Environment::Deftemplate.slot_range(deftemplate, :foo)
deftemplate.slot_range('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_cardinality` / `CLIPS::Environment::Deftemplate#slot_cardinality`

Returns the cardinality of a named slot of a deftemplate as integers or symbols (in case of infinity).

```ruby
CLIPS::Environment::Deftemplate.slot_cardinality(deftemplate, :foo)
deftemplate.slot_cardinality('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_existp` / `CLIPS::Environment::Deftemplate#slot_existp`

Returns a boolean for whether or not the slot with a given name
exists on the Deftemplate.

```ruby
CLIPS::Environment::Deftemplate.slot_existp(deftemplate, :foo)
deftemplate.slot_existp('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_singlep` / `CLIPS::Environment::Deftemplate#slot_singlep`

Returns a boolean for whether or not the slot with a given name
on the Deftemplate is a single slot.

```ruby
CLIPS::Environment::Deftemplate.slot_singlep(deftemplate, :foo)
deftemplate.slot_singlep('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_multip` / `CLIPS::Environment::Deftemplate#slot_multip`

Returns a boolean for whether or not the slot with a given name
on the Deftemplate is a multislot.

```ruby
CLIPS::Environment::Deftemplate.slot_multip(deftemplate, :foo)
deftemplate.slot_multip('bar')
```

#### `CLIPS::Environment::Deftemplate.slot_defaultp` / `CLIPS::Environment::Deftemplate#slot_defaultp`

Returns a symbol representing the type of default value a slot has
on the Deftemplate. Possible return values are as follows:

```ruby
:NO_DEFAULT
:STATIC_DEFAULT
:DYNAMIC_DEFAULT
```

```ruby
CLIPS::Environment::Deftemplate.slot_defaultp(deftemplate, :foo)
deftemplate.slot_defaultp('bar')
```

#### `CLIPS::Environment.find_deffacts` / `CLIPS::Environment#find_deffacts`

Finds a deffacts by name and returns a CLIPS::Environment::Deffacts object

```ruby
CLIPS::Environment.find_deffacts(:a)
env.find_deffacts("foo")
```

#### `CLIPS::Environment.find_deffunction` / `CLIPS::Environment#find_deffunction`

Finds a `Deffunction` by name and returns a CLIPS::Environment::Deffunction object

```ruby
CLIPS::Environment.find_deffunction(:"MAIN::my_deffunction")
env.find_deffunction("MAIN::other_deffunction")
```

### Deffacts Methods

#### `CLIPS::Environment::Deffacts.name` / `CLIPS::Environment::Deffacts#name`

Returns the name of a deffacts as a symbol

```ruby
CLIPS::Environment::Deffacts.name(deffacts)
deffacts.name
```

#### `CLIPS::Environment::Deffacts.pp_form` / `CLIPS::Environment::Deffacts#pp_form`

Returns a pretty printed string representation of the Deffacts

```ruby
CLIPS::Environment::Deffacts.pp_form(deffacts)
deffacts.pp_form
```

### Activation Methods

#### `CLIPS::Environment::Activation.delete` / `CLIPS::Environment::Activation#delete`

Deletes an `Activation` from the agenda.

```ruby
CLIPS::Environment::Activation.delete
activation.delete
```

#### `CLIPS::Environment::Activation.defrule_name` / `CLIPS::Environment::Activation#defrule_name`

Returns the name of a defrule that triggered this activation.

```ruby
CLIPS::Environment::Activation.defrule_name
activation.defrule_name
```

#### `CLIPS::Environment::Activation.get_salience` / `CLIPS::Environment::Activation#get_salience`

Returns the salience of this activation.

```ruby
CLIPS::Environment::Activation.get_salience(activation)
activation.get_salience
```

#### `CLIPS::Environment::Activation.set_salience` / `CLIPS::Environment::Activation#set_salience`

Sets the salience of this activation.

```ruby
CLIPS::Environment::Activation.set_salience(activation, 11)
activation.set_salience 123
```

#### `CLIPS::Environment::Activation.pp_form` / `CLIPS::Environment::Activation#pp_form`

Returns a pretty printed string representation of the Activation

```ruby
CLIPS::Environment::Activation.pp_form(activation)
activation.pp_form
```

### Deffunction Methods

#### `CLIPS::Environment::Deffunction.call` / `CLIPS::Environment::Deffunction#call` 

Calls the `Deffunction`, passing it the arguments, returning the return value

```ruby
CLIPS::Environment::Deffunction.call(deffunction, :FOO, :BAR)
deffunction.call(:FOO, :BAR)
```

#### `CLIPS::Environment::Deffunction.name` / `CLIPS::Environment::Deffunction#name` 

Returns the name of the `Deffunction` as a symbol

```ruby
CLIPS::Environment::Deffunction.name(deffunction)
deffunction.name
```

#### `CLIPS::Environment::Deffunction.pp_form` / `CLIPS::Environment::Deffunction#pp_form` 

Returns the pretty printed version of the `Deffunction`

```ruby
CLIPS::Environment::Deffunction.pp_form(deffunction)
deffunction.pp_form
```

#### `CLIPS::Environment::Deffunction.is_deletable` / `CLIPS::Environment::Deffunction#is_deletable` 

Returns whether the `Deffunction` is deletable or not

```ruby
CLIPS::Environment::Deffunction.is_deletable(deffunction)
deffunction.is_deletable
```

#### `CLIPS::Environment::Deffunction.defmodule` / `CLIPS::Environment::Deffunction#defmodule` 

Returns the `Defmodule` that the `Deffunction` is defined in

```ruby
CLIPS::Environment::Deffunction.defmodule(deffunction)
deffunction.defmodule
```

#### `CLIPS::Environment::Deffunction.defmodule_name` / `CLIPS::Environment::Deffunction#defmodule_name` 

Returns the name of the `Defmodule` that the `Deffunction` is defined in

```ruby
CLIPS::Environment::Deffunction.defmodule_name(deffunction)
deffunction.defmodule_name
```

## Running the tests

Simply do `rake compile` and then `rake test` in order to run the tests.
