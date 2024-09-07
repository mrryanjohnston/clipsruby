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

  def test_retract
    env = CLIPS.create_environment
    fact = env.assert_string("(bar 1 2)")
    facts = env.find_all_facts("(?f bar)")
    assert_equal 1, facts.length
    assert fact.retract
    facts = env.find_all_facts("(?f bar)")
    assert_equal 0, facts.length
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
  end

  def test_index
    env = CLIPS.create_environment
    assert_equal 1, env.assert_string("(foo bar baz)").index
    assert_equal 2, env.assert_string("(bar 1 2)").index
    assert_equal 3, env.assert_string("(baz \"asdf\")").index
  end
end
