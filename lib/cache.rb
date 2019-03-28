require 'hiredis'

# A generic purpose redis cache
class Cache
  # New cache instance
  def initialize
    @redis = Hiredis::Connection.new
    @redis.connect('localhost', 6379)
    @mutex = Mutex.new
  rescue Errno::ECONNREFUSED
    abort 'Can\'t connect to redis. ' \
          'Please make sure that redis is running on localhost:6379'
  end

  # fetches key on hit or writes and returns the result of the block on hit
  # @param key [String] cache key
  def fetch(key, &block)
    read(key) || yield.tap { |value| write(key, value) }
  end

  private

  def read(key)
    value = cache_action { @redis.write ['GET', key] }
    Marshal.load(value) if value
  end

  def write(key, value)
    value = Marshal.dump(value)
    cache_action { @redis.write ['SET', key, value] }
  end

  def cache_action
    @mutex.synchronize do
      yield
      @redis.read
    end
  end
end
