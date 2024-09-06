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
end
