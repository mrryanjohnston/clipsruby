require 'rake'

Gem::Specification.new do |s|
  s.name        = "clipsruby"
  s.version     = "0.0.4"
  s.summary     = "Calling CLIPS from within Ruby"
  s.description = "Calling the CLIPS programming language from within Ruby"
  s.authors     = ["Ryan Johnston"]
  s.email       = "mrryanjohnston@gmail.com"
  s.files = FileList['lib/**/*.rb', 'ext/**/*.{rb,c,h}']
  s.require_paths = ["lib", "lib/clipsruby"]
  s.extensions = ["ext/clipsruby/extconf.rb"]
  s.homepage    =
    "https://github.com/mrryanjohnston/clipsruby"
  s.license       = "MIT"
end
