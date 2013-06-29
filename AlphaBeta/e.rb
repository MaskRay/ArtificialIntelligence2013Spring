class A
  def add u, vs
    e[u].concat vs
  end

  def initialize
    branches = [[3], [2,2,3], [1,2,2,1,2,2,1],
                [2,1,1,1,2,1,1,1,1,1,2],
                [2,3,2,1,2,1,1,1,2,2,1,1,1,2],
                [2,2,1,3,2,2,2,2,2,2,2,2,1,1,2,2,2,2,2,2,2,1]]
    @internals = branches[0..-2].flatten.inject(&:+)+1
    branches = branches.flatten
    @values = [0,5,-3,3,3,-3,0,2,-2,3,5,2,5,-5,0,1,5,1,-3,0,-5,5,-3,3,2,3,-3,0,-1,-2,0,1,4,5,1,-1,-1,3,-3,2,-2]

    @e = Hash.new {|h,k| h[k] = [] }
    @dep = Hash.new

    dep, lo, hi = 0, 0, 1
    while hi <= branches.size
      hi2 = hi
      (lo...hi).each do |u|
        @dep[u] = dep
        @e[u].concat (hi2...hi2+branches[u]).to_a
        hi2 += branches[u]
      end
      lo, hi = hi, hi2
      dep += 1
    end

    #p @e
    #exit

    @v = Hash.new
    @cut = Hash.new
    minMax 0, 0
    alphaBeta 0, 0, -1000, 1000

    render
  end

  def minMax dep, u
    if dep == 6
      @v[u] = @values[u-@internals]
    elsif dep % 2 == 0
      res = -1000
      @e[u].each do |v|
        t = minMax dep+1, v
        res = t if t > res
      end
      @v[u] = res
    else
      res = 1000
      @e[u].each do |v|
        t = minMax dep+1, v
        res = t if t < res
      end
      @v[u] = res
    end
  end

  def alphaBeta dep, u, alpha, beta
      #alpha = -1000
      #beta = 1000
    if dep == 6
      @v[u] = @values[u-@internals]
    elsif dep % 2 == 0
      cut = false
      @e[u].each do |v|
        if cut
          @cut[v] = true
        else
          t = alphaBeta dep+1, v, alpha, beta
          alpha = t if t > alpha
          cut = true if alpha >= beta
        end
      end
      alpha
      #@v[u] = alpha
    else
      cut = false
      @e[u].each do |v|
        if cut
          @cut[v] = true
        else
          t = alphaBeta dep+1, v, alpha, beta
          beta = t if t < beta
          cut = true if alpha >= beta
        end
      end
      beta
      #@v[u] = beta
    end
  end

  def render
    puts 'graph {'

    @v.size.times do |u|
      #puts "#{u}[label=\"#{u} #{@v[u] ? @v[u] : 'x'}\",shape=#{@dep[u] && @dep[u] % 2 == 1 ? 'ellipse' : 'box'}]"
      puts "#{u}[label=\"#{@v[u] ? @v[u] : 'x'}\",shape=#{@dep[u] && @dep[u] % 2 == 1 ? 'ellipse' : 'box'}]"
    end
    (62..98).each do |u|
      #puts "#{u}[label=\"#{u} #{@v[u] ? @v[u] : 'x'}\",shape=#{@dep[u] && @dep[u] % 2 == 1 ? 'ellipse' : 'box'}]"
      puts "#{u}[label=\"#{@v[u] ? @v[u] : 'x'}\",shape=#{@dep[u] && @dep[u] % 2 == 1 ? 'ellipse' : 'box'}]"
    end

    onpath = [0,2,6,14,26,44,75]
    @v.size.times do |u|
      @e[u].each do |v|
        puts "#{u} -- #{v}[style=#{@cut[v] ? 'dotted' : 'filled'}, color=#{onpath.member?(u) && onpath.member?(v) ? 'blue' : 'black'}]"
      end
    end

    puts '}'
  end
end

A.new
