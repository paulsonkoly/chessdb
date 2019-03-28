require 'erb'

# Stores compiled erb files and runs them on request
class ERBStore
  def initialize
    @templates = {}
    @application = ERB.new(File.read('templates/application.html.erb'))
  end

  # Compiles an erb file and adds it to the erb store
  # @param token [Symbol] token for the erb template file
  # @param filename [String] path of the erb template file
  # @return [ERBStore] self
  def <<(token:, filename:)
    raise 'You cannot redefine the :application erb' if token == :application

    erb = ERB.new(File.read(filename))
    erb.filename = filename
    @templates[token] = erb
    self
  end

  # Puts the user template inside the main application.html.erb
  # @param token [Symbol] the same token as was used in {<<}
  # @param binding [Binding] evaluation binding
  def resolve_html(token, binding)
    receiver = binding.receiver
    wrap_with_application(receiver) { @templates[token].result(binding) }
  end

  private

  def wrap_with_application(receiver, &block)
    template = @application
    receiver.instance_eval { template.result(binding) }
  end
end
