require 'erb'

# Stores compiled erb files and runs them on request
class ERBStore
  def initialize
    @templates = {}
  end

  # Compiles an erb file and adds it to the erb store
  # @param token [Symbol] token for the erb template file
  # @param filename [String] path of the erb template file
  def <<(token:, filename:)
    erb = ERB.new(File.read(filename))
    erb.filename = filename
    @templates[token] = erb
  end

  # @param token [Symbl] the same token as was used in {<<}
  # @param binding [Binding] evaluation binding
  def resolve_html(token, binding)
    @templates[token].result(binding)
  end
end
