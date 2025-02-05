import numpy as np
import pandas as pd
import networkx as nx

def color_randomly(graph, p_blue=0.5, half=False, start_count_1=False):
### Color a graph randomly in red and blue, 
# if half=True, with one (if odd n) or two (if even n) more red than blue nodes,
# otherwise at random with a probability p for blue.
# and also count the number of blue and red nodes in the graph
    nr_blue_nodes = 0
    nr_red_nodes = 0
    n = graph.number_of_nodes()
    if half:
        if (n % 2) == 0: #n even
            nr_red = int((n+2)/2)
            nr_blue = int((n-2)/2)
        else:
            nr_red = int((n+1)/2)
            nr_blue = int((n-1)/2)
        colours = ['red']*nr_red+['blue']*nr_blue
        randomised_colours = np.random.permutation(colours)
    else:
        randomised_colours = []
        for node in range(n):
            colour = np.random.choice(['red', 'blue'], p=[1-p_blue, p_blue])
            randomised_colours.append(colour)
            if colour == 'red':
                nr_red_nodes +=1
            else:
                nr_blue_nodes +=1

    for node in graph.nodes():
        if start_count_1 == False: 
            graph.nodes[node]['color'] = randomised_colours[node]
        else: 
            graph.nodes[node]['color'] = randomised_colours[node-1] # -1 because randomised_colours starts at 0, while 'nodes' start at 1.
    return nr_red_nodes, nr_blue_nodes

def color_edges(graph):
### Colour the edges of a graph with coloured nodes according to the colour of the nodes.
### Also count the number of mixed edges in the graph (for homophily).
    edge_colors ={}
    nr_edges = 0
    nr_mixed_edges = 0
    for edge in graph.edges():
        nr_edges +=1
        #edge[0] and edge[1] are the nodes connected to this edge
        if graph.nodes[edge[0]]['color'] == graph.nodes[edge[1]]['color']: # If the two nodes have the same colour
            graph.edges[edge]['color'] = graph.nodes[edge[0]]['color']
            edge_colors[edge] = graph.nodes[edge[0]]['color']
        else:
            graph.edges[edge]['color'] = 'black'
            edge_colors[edge] = 'black'
            nr_mixed_edges += 1
    return nr_edges, nr_mixed_edges

def homopily(n, nr_red_nodes, nr_blue_nodes, nr_edges, nr_mixed_edges):
### Calculate expected number and measure real number of mixed edges.

    probability_mixed_edge = 2*nr_red_nodes*nr_blue_nodes/(n*(n-1))
    actual_fraction_mixed_edges = nr_mixed_edges / nr_edges
    # print(f'Probability of a mixed edge: {probability_mixed_edge}, actual fraction of mixed edges: {actual_fraction_mixed_edges}.')
    return probability_mixed_edge, actual_fraction_mixed_edges

def check_illusion_and_MSE(G, k=None):
### Checks whether there are Mm/Mw/Wm/Ww illusions in the graph, and measures the error of each nodes,
###  aggregates error into the mean squared error for different groups of nodes.
# If G is a regular graph, the degree can be indicated by k.    

    colors = nx.get_node_attributes(G, "color")
    nr_blue = list(colors.values()).count('blue')
    proportion_blue_global = nr_blue / G.number_of_nodes()
    if proportion_blue_global > 1/2:
        global_majority = 'blue'
    elif proportion_blue_global < 1/2:
        global_majority = 'red'
    else: global_majority = 'tie'
    # print(f'Global majority: {global_majority}')
    squared_errors = {}
    sum_squared_error_no_illusion = 0
    sum_squared_error_strict_illusion = 0
    sum_squared_error_any_illusion = 0
    nr_no_illusion = 0
    nr_strict_illusion = 0
    nr_any_illusion = 0

    for node in G.nodes():
        if k:
            degree = k
        else:
            degree = G.degree(node)

        count_blue = 0
        for nb in G.neighbors(node):     
            if G.nodes[nb]['color'] == 'blue':
                count_blue += 1
        if degree != 0:
            proportion_blue_local = count_blue / degree
        else: 
            proportion_blue_local = 1/2 #If a node has no neighbors, we say that half of them are blue and half red.
        error = proportion_blue_global - proportion_blue_local
        squared_error = error**2
        squared_errors[node] = squared_error
        
        if count_blue > degree/2: 
            local_majority = 'blue'
        elif count_blue == degree/2:
            local_majority = 'tie'
        else: # count_blue < degree/2
            local_majority = 'red'
        if local_majority == global_majority:
            node_under_illusion = 'false'
            sum_squared_error_no_illusion += squared_error
            nr_no_illusion +=1
        elif local_majority == 'tie' or global_majority == 'tie':
            node_under_illusion = 'weak'
            sum_squared_error_any_illusion += squared_error
            nr_any_illusion +=1
        else: 
            node_under_illusion = 'strict'
            sum_squared_error_any_illusion += squared_error
            sum_squared_error_strict_illusion += squared_error
            nr_any_illusion +=1
            nr_strict_illusion +=1

    mean_squared_error = sum(squared_errors.values())/len(squared_errors)
    if nr_no_illusion != 0:
        mean_squared_error_no_illusion = sum_squared_error_no_illusion/nr_no_illusion
    else: 
        mean_squared_error_no_illusion = 'NotApplicable'
    if nr_any_illusion !=0:
        mean_squared_error_any_illusion = sum_squared_error_any_illusion/nr_any_illusion
    else: 
        mean_squared_error_any_illusion = 'NotApplicable'
    if nr_strict_illusion !=0:
        mean_squared_error_strict = sum_squared_error_strict_illusion/nr_strict_illusion
    else:
        mean_squared_error_strict = 'NotApplicable'

    global_tie = False
    if global_majority == 'tie':
        global_tie = True
    if nr_strict_illusion > G.number_of_nodes()/2:
        # print('The network has a majority-majority-illusion.')
        return 'Mmi', 'Mwmi', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion
    elif nr_strict_illusion == G.number_of_nodes()/2:
        if nr_any_illusion> G.number_of_nodes()/2:
            return 'WMmi', 'Mwmi', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion
        else: #nr weak <= |V|/2 but strict -> weak so: nr weak = |V|/2
            return 'WMmi', 'WMwmi', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion
    else: # nr strict < |V|/2 : no strict illusion
        if nr_any_illusion > G.number_of_nodes()/2:
            return 'no_strict', 'Mwmi', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion
        elif nr_any_illusion == G.number_of_nodes()/2: 
            return 'no_strict', 'WMwmi', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion
        else:
            return 'no_strict', 'no_weak', nr_strict_illusion, nr_any_illusion, proportion_blue_global, mean_squared_error, mean_squared_error_no_illusion, mean_squared_error_any_illusion, mean_squared_error_strict, sum_squared_error_no_illusion, sum_squared_error_any_illusion, sum_squared_error_strict_illusion

def to_float_list(x):
### Helper function to read strings as numbers
    if type(x) != str:
        # print(f' Type: {type(x)}, x: {x}')
        x = []
    if type(x) == str:
        x = x.replace("[", "").replace("]", "").split(', ')
        x = list(map(float, x))
    return x



def calculate_aggregated_values(data):
### Calculate average values for the properties that contain a value per node:

    # Eigenvector centrality:
    data['EV_centr'] = data['EV_centr'].apply(to_float_list)
    C_EV =[]
    for run in range(len(data)):
        if data.EV_centr[run]:
            n = data.n[run]
            max_EV_centr = max(data.EV_centr[run])
            sum_differences = 0
            for node in range(n):
                difference = max_EV_centr - data.EV_centr[run][node]
                sum_differences += difference
            C_EV.append(sum_differences)
        else:
            C_EV.append('NA')
    data['C_EV'] = C_EV
    #NB! for EV-centrality we don't know the denominator (yet), hence the values are not normalized. However, to measure correlation this does not matter.

    # Closeness centrality:
    data['close_centr'] = data['close_centr'].apply(to_float_list)
    C_closeness =[]
    for run in range(len(data)):
        n = data.n[run]
        max_close_centr = max(data.close_centr[run])
        sum_differences = 0
        for node in range(n):
            difference = max_close_centr - data.close_centr[run][node]
            sum_differences += difference
        C_closeness.append(sum_differences/(n-1))
    data['C_closeness'] = C_closeness

    # Betweenness centrality:
    data['between_centr'] = data['between_centr'].apply(to_float_list)
    C_between =[]
    for run in range(len(data)):
        n = data.n[run]
        max_between_centr = max(data.between_centr[run])
        sum_differences = 0
        for node in range(n):
            difference = max_between_centr - data.between_centr[run][node]
            sum_differences += difference
        C_between.append(sum_differences/(n-1))
    data['C_between'] = C_between

    # Degree sequence
    data['deg_seq'] = data['deg_seq'].apply(to_float_list)
    C_degree =[]
    for run in range(len(data)):
        n = data.n[run]
        max_degree = max(data.deg_seq[run])
        sum_differences = 0
        for node in range(n):
            difference = max_degree - data.deg_seq[run][node]
            sum_differences += difference
        C_degree.append(sum_differences/(n**2-3*n+2))
    data['C_degree'] = C_degree
    return data