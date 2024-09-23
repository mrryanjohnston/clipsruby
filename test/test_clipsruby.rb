require "minitest/autorun"
require "clipsruby"

class ClipsrubyTest < Minitest::Test
  def test_eval_to_h
    env = CLIPS.create_environment
    assert_equal 1,
      env._eval("(- 3 2)")
    assert_equal :asdf,
      env._eval("(sym-cat as \"df\")")
    assert_equal "hjkl",
      env._eval("(str-cat \"hj\" kl)")
    assert_equal [1, "2", :four],
      env._eval("(create$ 1 \"2\" four)")
    assert_equal({ implied: [ :five, 6 ] },
      env._eval("(assert (foo five 6))").to_h)
  end

  def test_build_assert_hash_to_h_deftemplate_name
    env = CLIPS.create_environment
    assert_nil env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
    assert_equal({ foo: :asdf, bar: [ 1, 2, "hjkl" ] },
      fact.to_h)
    assert_equal(:"my-template", fact.deftemplate_name)
  end

  def test_assert_string_find_fact
    env = CLIPS.create_environment
    fact1 = env.assert_string("(foo bar baz)")
    fact2 = env.assert_string("(bar baz 1)")
    env.assert_string("(bar baz 2)")
    env.assert_string("(bar baz 3)")
    facts = env.find_all_facts("(?f foo) (?b bar)", "(eq (nth$ 2 ?f:implied) (nth$ 1 ?b:implied))")
    assert_equal 6, facts.length
    facts = env.find_fact("(?f foo) (?b bar)", "(eq (nth$ 2 ?f:implied) (nth$ 1 ?b:implied))")
    assert_equal 2, facts.length
    assert_equal([{ implied: [ :bar, :baz ] }, { implied: [ :baz, 1 ] }],
      facts.map(&:to_h))
    assert_equal(fact1.to_h,
      facts[0].to_h)
    assert_equal(fact2.to_h,
      facts[1].to_h)
    assert_nil env.assert_string("(something is wrong ?)")
  end

  def test_assert_string_find_all_facts
    env = CLIPS.create_environment
    fact = env.assert_string("(foo bar baz)")
    env.assert_string("(bar 1 2)")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length
    assert_equal({ implied: [ :bar, :baz ] },
      fact.to_h)
    assert_equal(fact.to_h,
      facts[0].to_h)
    env.assert_string("(foo 1 2)")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 2, facts.length
    facts = env.find_all_facts("(?f bar)")
    assert_equal 1, facts.length
    facts = env.find_all_facts("(?f foo)", "(eq (nth$ 1 ?f:implied) bar)")
    assert_equal 1, facts.length
  end

  def test_build_run
    env = CLIPS.create_environment
    env.build("(defrule runs-once =>)")
    env.build("(defrule runs-once-also =>)")
    env.build("(defrule runs-once-as-well =>)")
    env.build("(defrule runs-once-as-well-too =>)")
    assert_equal 1, env.run(1)
    assert_equal 3, env.run
  end

  def test_batch_star
    env = CLIPS.create_environment
    refute env.batch_star("test/file_does_not_exist.bat")
    assert env.batch_star("test/batch_star.bat")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 2, facts.length
    facts = env.find_all_facts("(?f bar)")
    assert_equal 1, facts.length
  end

  def test_clear
    env = CLIPS.create_environment
    env.build("(defrule runs-once =>)")
    assert_nil env.build("(deftemplate my-template (slot foo) (multislot bar))")
    env.assert_string("(bar 1 2)")
    env.clear
    refute env.find_all_facts("(?f bar)")
    assert_equal 0, env.run
    assert_nil env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
  end

  def test_reset
    env = CLIPS.create_environment
    env.build("(deffacts my-facts (foo bar) (foo bat) (foo zig))")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 0, facts.length
    env.reset
    facts = env.find_all_facts("(?f foo)")
    assert_equal 3, facts.length
    env.build("(defrule do => (assert (foo zap)))")
    assert 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 4, facts.length
    env.reset
    facts = env.find_all_facts("(?f foo)")
    assert_equal 3, facts.length
    assert 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 4, facts.length
  end

  def test_save_load
    env = CLIPS.create_environment
    env.build("(defrule do => (assert (foo zap)))")
    env.save("test/load.clp")
    env.clear
    refute env.load("test/file_does_not_exist.bat")
    assert env.load("test/load.clp")
    env.reset
    assert_equal 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length

    env = CLIPS.create_environment
    assert env.load("test/load.clp")
    env.reset
    assert_equal 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length
  end

  def test_save_facts_load_facts
    env = CLIPS.create_environment
    env.assert_string("(foo bar baz)")
    env.assert_string("(bar 1 2)")
    env.assert_string("(bar \"asdf\")")
    env.save_facts("test/load_facts.clp")
    env.clear
    refute env.load_facts("test/file_does_not_exist.bat")
    assert env.load_facts("test/load_facts.clp")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length
    facts = env.find_all_facts("(?f bar)")
    assert_equal 2, facts.length
  end

  def test_bsave_bload
    env = CLIPS.create_environment
    env.build("(defrule do => (assert (foo zap)))")
    env.bsave("test/bload.bin")
    env.clear
    refute env.bload("test/file_does_not_exist.bat")
    assert env.bload("test/bload.bin")
    env.reset
    assert_equal 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length

    env = CLIPS.create_environment
    assert env.bload("test/bload.bin")
    env.reset
    assert_equal 1, env.run
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length
  end

  def test_bsave_facts_bload_facts
    env = CLIPS.create_environment
    env.assert_string("(foo bar baz)")
    env.assert_string("(bar 1 2)")
    env.assert_string("(bar \"asdf\")")
    env.bsave_facts("test/bload_facts.bin")
    env.clear
    refute env.bload_facts("test/file_does_not_exist.bat")
    assert env.bload_facts("test/bload_facts.bin")
    facts = env.find_all_facts("(?f foo)")
    assert_equal 1, facts.length
    facts = env.find_all_facts("(?f bar)")
    assert_equal 2, facts.length
  end

  def test_slot_names
    env = CLIPS.create_environment
    assert_nil env.build("(deftemplate my-template (slot foo) (multislot bar))")
    assert_equal [ :foo, :bar ],
      env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ]).slot_names
  end

  def test_get_slot
    env = CLIPS.create_environment
    assert_nil env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
    assert_equal :asdf,
      fact.get_slot(:foo)
    assert_equal [ 1, 2, "hjkl" ],
      fact.get_slot('bar')
  end

  def test_fact_pp_form
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    assert_equal "(my-template \n   (foo asdf) \n   (bar 1 2 \"hjkl\"))",
      env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ]).pp_form
  end

  def test_retract
    env = CLIPS.create_environment
    fact = env.assert_string("(bar 1 2)")
    facts = env.find_all_facts("(?f bar)")
    assert_equal 1, facts.length
    assert fact.retract
    facts = env.find_all_facts("(?f bar)")
    assert_equal 0, facts.length
  end

  def test_fact_existp
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
    assert fact.existp
    fact.retract
    refute fact.existp
  end

  def test_modify
    env = CLIPS.create_environment
    assert_nil env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
    assert_equal :asdf,
      fact.get_slot(:foo)
    assert_equal [ 1, 2, "hjkl" ],
      fact.get_slot('bar')
    facts = env.find_all_facts("(?f my-template)")
    assert_equal 1, facts.length
    fact.modify(bar: ["a new value!"])
    assert_equal ["a new value!"],
      fact.get_slot('bar')
    fact = fact.modify(foo: 67.8)
    assert_equal 67.8,
      fact.get_slot('foo')
    facts = env.find_all_facts("(?f my-template)")
    assert_equal 1, facts.length
    fact = fact.modify("foo": 88.88)
    assert_equal 88.88,
      fact.get_slot('foo')
    fact = fact.modify({"foo" => "one two three"})
    assert_equal "one two three",
      fact.get_slot('foo')
  end

  def test_index
    env = CLIPS.create_environment
    assert_equal 1, env.assert_string("(foo bar baz)").index
    assert_equal 2, env.assert_string("(bar 1 2)").index
    assert_equal 3, env.assert_string("(baz \"asdf\")").index
  end

  def test_find_defrule_name
    env = CLIPS.create_environment
    env.build("(defrule runs-once =>)")
    env.build("(defrule runs-once-also =>)")
    assert_equal :"runs-once",
      env.find_defrule(:"runs-once").name
    assert_equal :"runs-once-also",
      env.find_defrule(:"runs-once-also").name
  end

  def test_defrule_pp_form
    env = CLIPS.create_environment
    env.build("(defrule a =>)")
    assert_equal "(defrule MAIN::a\n   =>)\n",
      env.find_defrule(:a).pp_form
    env.build("(defrule foo (asdf ?asdf) => (println \"It says: \" ?asdf))")
    assert_equal "(defrule MAIN::foo\n   (asdf ?asdf)\n   =>\n   (println \"It says: \" ?asdf))\n",
      env.find_defrule(:foo).pp_form
  end

  def test_defrule_is_deletable
    env = CLIPS.create_environment
    env.build("(defrule a =>)")
    assert env.find_defrule(:a).is_deletable
  end

  def test_has_breakpoint_set_break_remove_break
    env = CLIPS.create_environment
    env.build("(defrule a =>)")
    defrule = env.find_defrule(:a)
    refute defrule.has_breakpoint
    refute defrule.remove_break
    defrule.set_break
    assert defrule.has_breakpoint
    assert defrule.remove_break
    refute defrule.has_breakpoint
  end

  def test_defrule_salience
    env = CLIPS.create_environment
    env.build("(defrule with-salience (declare (salience 1000)) =>)")
    env.build("(defrule without-salience =>)")
    assert_equal 1000,
      env.find_defrule(:"with-salience").salience
    assert_equal 0,
      env.find_defrule(:"without-salience").salience
  end

  def test_find_defmodule_name
    env = CLIPS.create_environment
    assert_nil env.find_defmodule(:does_not_exist)
    env.build("(defmodule my_module (export ?ALL))")
    env.build("(defmodule also-my-module (export ?ALL))")
    assert_equal :my_module,
      env.find_defmodule(:my_module).name
    assert_equal :"also-my-module",
      env.find_defmodule(:"also-my-module").name
  end

  def test_find_defmodule_pp_form
    env = CLIPS.create_environment
    env.build("(defmodule my_module (export ?ALL))")
    assert_equal "(defmodule my_module\n   (export ?ALL))\n",
      env.find_defmodule(:my_module).pp_form
  end

  def test_get_current_module_set_current_module
    env = CLIPS.create_environment
    assert_equal :MAIN,
      env.get_current_module.name
    env.build("(defmodule my_module (export ?ALL))")
    assert_equal :my_module,
      env.get_current_module.name
    defmodule = env.find_defmodule(:MAIN)
    assert_equal defmodule,
      env.set_current_module(defmodule)
    assert_equal :MAIN,
      env.get_current_module.name
  end

  def test_defmodule_set_current
    env = CLIPS.create_environment
    env.build("(defmodule my_module (export ?ALL))")
    defmodule = env.find_defmodule(:MAIN)
    assert_equal defmodule,
      defmodule.set_current
    assert_equal :MAIN,
      env.get_current_module.name
  end

  def test_get_fact_list
    env = CLIPS.create_environment
    fact1 = env.assert_string("(foo bar baz)")
    fact2 = env.assert_string("(bar baz 1)")
    assert_equal [fact1.to_h, fact2.to_h],
      env.get_fact_list.map(&:to_h)
    env.build("(defmodule my_module (export ?ALL))")
    fact3 = env.assert_string("(baz \"asdf\")")
    assert_equal [fact1.to_h, fact2.to_h, fact3.to_h],
      env.get_fact_list.map(&:to_h)
    assert_equal [fact1.to_h, fact2.to_h],
      env.get_fact_list(:MAIN).map(&:to_h)
    assert_equal [fact3.to_h],
      env.get_fact_list(:my_module).map(&:to_h)
    defmodule = env.get_current_module
    assert_equal [fact3.to_h],
      env.get_fact_list(defmodule).map(&:to_h)
    assert_equal [fact3.to_h],
      defmodule.get_fact_list.map(&:to_h)
    defmodule = env.find_defmodule(:MAIN)
    assert_equal [fact1.to_h, fact2.to_h],
      env.get_fact_list(defmodule).map(&:to_h)
    assert_equal [fact1.to_h, fact2.to_h],
      defmodule.get_fact_list.map(&:to_h)
  end

  def test_find_deftemplate_name_pp_form
    env = CLIPS.create_environment
    refute env.find_deftemplate(:does_not_exist)
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    template = env.find_deftemplate(:"my-template")
    assert_equal :"my-template",
      template.name
    assert_equal "(deftemplate MAIN::my-template\n   (slot foo)\n   (multislot bar))\n",
      template.pp_form
  end

  def test_find_deffacts_name_pp_form
    env = CLIPS.create_environment
    env.build("(deffacts my-facts (foo bar) (foo bat) (foo zig))")
    refute env.find_deffacts(:foo)
    deffacts = env.find_deffacts(:"my-facts")
    assert_equal :"my-facts",
      deffacts.name
    assert_equal "(deffacts MAIN::my-facts\n   (foo bar)\n   (foo bat)\n   (foo zig))\n",
      deffacts.pp_form
  end

  def test_fact_deftemplate
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.assert_hash(:"my-template", foo: :asdf, bar: [ 1, 2, "hjkl" ])
    assert_equal :"my-template",
      fact.deftemplate.name
  end

  def test_deftemplate_assert_hash
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    fact = env.find_deftemplate(:"my-template").assert_hash(foo: :asdf, bar: [1, 2, "hjkl"])
    assert_equal :"my-template",
      fact.deftemplate_name
    assert_equal :"my-template",
      fact.deftemplate.name
    assert_equal({ foo: :asdf, bar: [ 1, 2, "hjkl" ] },
      fact.to_h)
  end

  def test_get_deftemplate_list
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    env.build("(defmodule my_module (export deftemplate another_template))")
    env.build("(deftemplate another_template (slot baz) (multislot bat))")
    assert_equal [ :"MAIN::my-template", :"my_module::another_template" ],
      env.get_deftemplate_list
    assert_equal [ :another_template ],
      env.find_defmodule(:my_module).get_deftemplate_list
  end

  def test_get_defrule_list
    env = CLIPS.create_environment
    env.build("(defrule runs-once =>)")
    env.build("(defrule runs-once-also =>)")
    env.build("(defrule runs-once-as-well =>)")
    env.build("(defrule runs-once-as-well-too =>)")
    assert_equal [ :"MAIN::runs-once", :"MAIN::runs-once-also", :"MAIN::runs-once-as-well", :"MAIN::runs-once-as-well-too" ],
      env.get_defrule_list
    env.build("(defmodule my_module (export ?ALL))")
    env.build("(defrule rule-in-another-module =>)")
    my_module = env.find_defmodule(:my_module)
    assert_equal [ :"MAIN::runs-once", :"MAIN::runs-once-also", :"MAIN::runs-once-as-well", :"MAIN::runs-once-as-well-too", :"my_module::rule-in-another-module" ],
      env.get_defrule_list
    assert_equal [ :"rule-in-another-module" ],
      env.get_defrule_list(my_module)
    assert_equal [ :"rule-in-another-module" ],
      my_module.get_defrule_list
    assert_equal [ :"rule-in-another-module" ],
      env.get_defrule_list(:my_module)
  end

  def test_deftemplate_defmodule_name
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    assert_equal :MAIN,
      env.find_deftemplate(:"my-template").defmodule_name
    env.build("(defmodule my_module (export deftemplate another_template))")
    env.build("(deftemplate another_template (slot baz) (multislot bat))")
    assert_equal :my_module,
      env.find_deftemplate(:another_template).defmodule_name
  end

  def test_deftemplate_slot_names
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    env.build("(deftemplate another_template (slot baz) (multislot bat))")
    assert_equal [:foo, :bar],
      env.find_deftemplate(:"my-template").slot_names
    assert_equal [:baz, :bat],
      env.find_deftemplate(:another_template).slot_names
  end

  def test_deftemplate_is_deletable
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    env.build("(deftemplate another_template (slot baz) (multislot bat))")
    assert env.find_deftemplate(:"my-template").is_deletable
    env.assert_hash(:another_template, baz: 1)
    refute env.find_deftemplate(:another_template).is_deletable
  end

  def test_deftemplate_is_implied
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bar))")
    refute env.find_deftemplate(:"my-template").is_implied
    assert env.assert_string("(asdf hjkl)").deftemplate.is_implied
  end

  def test_slot_allowed_values
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo (allowed-values a b c d)) (multislot bar (allowed-values 1 2 asdf)) (slot baz) (multislot bat))")
    template = env.find_deftemplate(:"my-template")
    assert_equal [ :a, :b, :c, :d ],
      template.slot_allowed_values(:foo)
    assert_equal [ 1, 2, :asdf ],
      template.slot_allowed_values('bar')
    refute template.slot_allowed_values('baz')
    refute template.slot_allowed_values('bat')
  end

  def test_slot_default_value
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo (default FOO)) (multislot bar) (slot baz) (multislot bat (default 1 2 3)))")
    template = env.find_deftemplate(:"my-template")
    assert_equal :FOO,
      template.slot_default_value(:foo)
    assert_equal [],
      template.slot_default_value('bar')
    assert_equal :nil,
      template.slot_default_value('baz')
    assert_equal [1, 2, 3],
      template.slot_default_value('bat')
  end

  def test_slot_types
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo (type INTEGER SYMBOL)) (multislot bat (type FLOAT FACT-ADDRESS)))")
    template = env.find_deftemplate(:"my-template")
    assert_equal [:INTEGER, :SYMBOL],
      template.slot_types(:foo)
    assert_equal [:FLOAT, :"FACT-ADDRESS"],
      template.slot_types(:bat)
  end

  def test_slot_range
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo (type INTEGER) (range 1 123)) (slot bat (type FACT-ADDRESS)) (slot bar (type FLOAT)))")
    template = env.find_deftemplate(:"my-template")
    assert_equal [1, 123],
      template.slot_range(:foo)
    refute template.slot_range(:bat)
    assert_equal [:"-oo", :"+oo"],
      template.slot_range(:bar)
  end

  def test_slot_cadinality
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo) (multislot bat (cardinality 3 7)) (multislot bar (cardinality 88 ?VARIABLE)))")
    template = env.find_deftemplate(:"my-template")
    assert_equal [],
      template.slot_cardinality(:foo)
    assert_equal [3, 7],
      template.slot_cardinality(:bat)
    assert_equal [88, :"+oo"],
      template.slot_cardinality(:bar)
  end

  def test_deftemplate_slot_existp_multip_singlep_defaultp
    env = CLIPS.create_environment
    env.build("(deftemplate my-template (slot foo (default FOO)) (multislot bar) (slot bat (default-dynamic (gensym))) (slot baz (default ?NONE)))")
    assert env.find_deftemplate(:"my-template").slot_existp(:foo)
    assert env.find_deftemplate(:"my-template").slot_singlep(:foo)
    refute env.find_deftemplate(:"my-template").slot_multip(:foo)
    refute env.find_deftemplate(:"my-template").slot_singlep(:bar)
    assert env.find_deftemplate(:"my-template").slot_multip(:bar)
    assert_equal :STATIC_DEFAULT,
      env.find_deftemplate(:"my-template").slot_defaultp(:foo)
    assert_equal :STATIC_DEFAULT,
      env.find_deftemplate(:"my-template").slot_defaultp(:bar)
    assert_equal :NO_DEFAULT,
      env.find_deftemplate(:"my-template").slot_defaultp(:baz)
    assert_equal :DYNAMIC_DEFAULT,
      env.find_deftemplate(:"my-template").slot_defaultp(:bat)
  end

  def test_watch_unwatch
    env = CLIPS.create_environment
    assert_nil env.get_watch_state(:foo)

    refute env.get_watch_state(:all)
    assert_nil env.watch_all
    #TODO: uncomment when fixed in upstream CLIPS
    #assert env.get_watch_state(:all)
    assert_nil env.unwatch_all
    refute env.get_watch_state(:all)
    assert_nil env.watch(:all)
    #TODO: uncomment when fixed in upstream CLIPS
    #assert env.get_watch_state(:all)
    assert_nil env.unwatch(:all)
    refute env.get_watch_state(:all)

    refute env.get_watch_state(:facts)
    assert_nil env.watch_facts
    assert env.get_watch_state(:facts)
    assert_nil env.unwatch_facts
    refute env.get_watch_state(:facts)
    assert_nil env.watch(:facts)
    assert env.get_watch_state(:facts)
    assert_nil env.unwatch(:facts)
    refute env.get_watch_state(:facts)

    refute env.get_watch_state(:instances)
    assert_nil env.watch_instances
    assert env.get_watch_state(:instances)
    assert_nil env.unwatch_instances
    refute env.get_watch_state(:instances)
    assert_nil env.watch(:instances)
    assert env.get_watch_state(:instances)
    assert_nil env.unwatch(:instances)
    refute env.get_watch_state(:instances)

    refute env.get_watch_state(:slots)
    assert_nil env.watch_slots
    assert env.get_watch_state(:slots)
    assert_nil env.unwatch_slots
    refute env.get_watch_state(:slots)
    assert_nil env.watch(:slots)
    assert env.get_watch_state(:slots)
    assert_nil env.unwatch(:slots)
    refute env.get_watch_state(:slots)

    refute env.get_watch_state(:rules)
    assert_nil env.watch_rules
    assert env.get_watch_state(:rules)
    assert_nil env.unwatch_rules
    refute env.get_watch_state(:rules)
    assert_nil env.watch(:rules)
    assert env.get_watch_state(:rules)
    assert_nil env.unwatch(:rules)
    refute env.get_watch_state(:rules)

    refute env.get_watch_state(:activations)
    assert_nil env.watch_activations
    assert env.get_watch_state(:activations)
    assert_nil env.unwatch_activations
    refute env.get_watch_state(:activations)
    assert_nil env.watch(:activations)
    assert env.get_watch_state(:activations)
    assert_nil env.unwatch(:activations)
    refute env.get_watch_state(:activations)

    refute env.get_watch_state(:messages)
    assert_nil env.watch_messages
    assert env.get_watch_state(:messages)
    assert_nil env.unwatch_messages
    refute env.get_watch_state(:messages)
    assert_nil env.watch(:messages)
    assert env.get_watch_state(:messages)
    assert_nil env.unwatch(:messages)
    refute env.get_watch_state(:messages)

    refute env.get_watch_state(:message_handlers)
    assert_nil env.watch_message_handlers
    assert env.get_watch_state(:message_handlers)
    assert_nil env.unwatch_message_handlers
    refute env.get_watch_state(:message_handlers)
    assert_nil env.watch(:message_handlers)
    assert env.get_watch_state(:message_handlers)
    assert_nil env.unwatch(:message_handlers)
    refute env.get_watch_state(:message_handlers)

    refute env.get_watch_state(:generic_functions)
    assert_nil env.watch_generic_functions
    assert env.get_watch_state(:generic_functions)
    assert_nil env.unwatch_generic_functions
    refute env.get_watch_state(:generic_functions)
    assert_nil env.watch(:generic_functions)
    assert env.get_watch_state(:generic_functions)
    assert_nil env.unwatch(:generic_functions)
    refute env.get_watch_state(:generic_functions)

    refute env.get_watch_state(:methods)
    assert_nil env.watch_methods
    assert env.get_watch_state(:methods)
    assert_nil env.unwatch_methods
    refute env.get_watch_state(:methods)
    assert_nil env.watch(:methods)
    assert env.get_watch_state(:methods)
    assert_nil env.unwatch(:methods)
    refute env.get_watch_state(:methods)

    refute env.get_watch_state(:deffunctions)
    assert_nil env.watch_deffunctions
    assert env.get_watch_state(:deffunctions)
    assert_nil env.unwatch_deffunctions
    refute env.get_watch_state(:deffunctions)
    assert_nil env.watch(:deffunctions)
    assert env.get_watch_state(:deffunctions)
    assert_nil env.unwatch(:deffunctions)
    refute env.get_watch_state(:deffunctions)

    refute env.get_watch_state(:compilations)
    assert_nil env.watch_compilations
    assert env.get_watch_state(:compilations)
    assert_nil env.unwatch_compilations
    refute env.get_watch_state(:compilations)
    assert_nil env.watch(:compilations)
    assert env.get_watch_state(:compilations)
    assert_nil env.unwatch(:compilations)
    refute env.get_watch_state(:compilations)

    refute env.get_watch_state(:statistics)
    assert_nil env.watch_statistics
    assert env.get_watch_state(:statistics)
    assert_nil env.unwatch_statistics
    refute env.get_watch_state(:statistics)
    assert_nil env.watch(:statistics)
    assert env.get_watch_state(:statistics)
    assert_nil env.unwatch(:statistics)
    refute env.get_watch_state(:statistics)

    refute env.get_watch_state(:globals)
    assert_nil env.watch_globals
    assert env.get_watch_state(:globals)
    assert_nil env.unwatch_globals
    refute env.get_watch_state(:globals)
    assert_nil env.watch(:globals)
    assert env.get_watch_state(:globals)
    assert_nil env.unwatch(:globals)
    refute env.get_watch_state(:globals)

    refute env.get_watch_state(:focus)
    assert_nil env.watch_focus
    assert env.get_watch_state(:focus)
    assert_nil env.unwatch_focus
    refute env.get_watch_state(:focus)
    assert_nil env.watch(:focus)
    assert env.get_watch_state(:focus)
    assert_nil env.unwatch(:focus)
    refute env.get_watch_state(:focus)
  end
end
