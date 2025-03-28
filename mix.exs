defmodule Supernova.MixProject do
  use Mix.Project

  def project do
    [
      app: :supernova,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:public_key, :logger],
      mod: {Supernova.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ankh, "0.17.0"},
      {:thousand_island, "~> 1.0-pre"},
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:credo, ">= 0.0.0", only: :dev},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
