defmodule ExMCP.Content.SchemaDNS do
  @moduledoc false

  alias ExMCP.Internal.DNSResolver

  @spec resolve(String.t(), non_neg_integer()) ::
          {:ok, [:inet.ip_address()]} | {:error, :dns_failed | :dns_timeout}
  def resolve(host, timeout_ms) when is_binary(host) and is_integer(timeout_ms) do
    DNSResolver.resolve(host, timeout_ms)
  end
end
