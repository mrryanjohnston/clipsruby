require "rake/extensiontask"
require "rake/testtask"

Rake::ExtensionTask.new "clipsruby" do |ext|
  ext.lib_dir = "lib/clipsruby"
end

Rake::TestTask.new do |t|
  t.libs << "test"
end

desc "Run tests"
task default: :test
