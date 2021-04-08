sections = [
  %{
    "title" => "Getting started",
    "reset_lesson_position" => false,
    "lessons" => [
      %{"name" => "Welcome"},
      %{"name" => "Installation"}
    ]
  },
  %{
    "title" => "Basic operator",
    "reset_lesson_position" => false,
    "lessons" => [
      %{"name" => "Addition / Subtraction"},
      %{"name" => "Multiplication / Division"}
    ]
  },
  %{
    "title" => "Advanced topics",
    "reset_lesson_position" => true,
    "lessons" => [
      %{"name" => "Mutability"},
      %{"name" => "Immutability"}
    ]
  }
]

defmodule GenWalk do
  def get(a, path) do
    {_, %{got: got}} =
      walk(a, %{got: nil}, fn
        a, m, acc -> if m.path == path, do: {a, %{got: a}}, else: {a, acc}
      end)

    got
  end

  def walk(a, acc, f, opts \\ [], meta \\ %{path: "root", lv: 1}) do
    {a_, acc_} = f.(a, meta, acc)
    {_a_, _acc_} = walk_(a_, acc_, f, opts, meta)
  end

  def walk_(a, acc, f, opts, meta) when is_map(a) do
    a
    |> Enum.with_index()
    |> Enum.reduce({%{}, acc}, fn {{k, v}, _i}, {a_, acc_} ->
      next_meta = %{path: "#{meta.path}[#{k}]", lv: meta.lv + 1}

      {v_, acc_} = walk(v, acc_, f, opts, next_meta)
      {Map.put(a_, k, v_), acc_}
    end)
  end

  def walk_(a, acc, f, opts, meta) when is_list(a) do
    a
    |> Enum.with_index()
    |> Enum.map_reduce(acc, fn {v, i}, acc_ ->
      next_meta = %{path: "#{meta.path}[][#{i}]", lv: meta.lv}
      {_v_, _acc_} = walk(v, acc_, f, opts, next_meta)
    end)
  end

  def walk_(a, acc, _f, _opts, _meta), do: {a, acc}
end

update_pos = fn acc, lv, pos when is_function(pos, 1) ->
  update_in(acc, [Access.key(lv, %{pos: 0})], fn lv ->
    %{lv | pos: pos.(lv.pos)}
  end)
end

{result, _} =
  GenWalk.walk(sections, %{}, fn
    %{"reset_lesson_position" => true} = a, m, acc when is_map(a) ->
      acc = update_pos.(acc, m.lv, fn pos -> pos + 1 end)
      acc = update_pos.(acc, m.lv + 1, fn _ -> 0 end)
      {Map.put(a, "position", acc[m.lv].pos), acc}

    a, m, acc when is_map(a) ->
      acc = update_pos.(acc, m.lv, fn pos -> pos + 1 end)
      {Map.put(a, "position", acc[m.lv].pos), acc}

    a, _m, acc ->
      {a, acc}
  end)

welcome = GenWalk.get(result, "root[][0][lessons][][0]")
install = GenWalk.get(result, "root[][0][lessons][][1]")
addition = GenWalk.get(result, "root[][1][lessons][][0]")
multiplication = GenWalk.get(result, "root[][1][lessons][][1]")
mutability = GenWalk.get(result, "root[][2][lessons][][0]")
immutability = GenWalk.get(result, "root[][2][lessons][][1]")
true = welcome["position"] == 1
true = install["position"] == 2
true = addition["position"] == 3
true = multiplication["position"] == 4
true = mutability["position"] == 1
true = immutability["position"] == 2
IO.inspect(result, limit: :infinity)
