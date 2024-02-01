# Code for computational experiments from the paper
# 'On the Graph Theory of Majority Illusions: Theoretical Results and Computational Experiments'
# by Maaike Venema-Los (University of Groningen, m.d.los@rug.nl), Zoé Christoff, and Davide Grossi.

import numpy as np
import pandas as pd
import networkx as nx

def color_randomly(graph, p_blue=0.5, half=False, start_count_1=False):
    #Color a graph randomly in red and blue, 
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
    # Colour the edges of a graph with coloured nodes according to the colour of the nodes.
    # and also count the number of mixed edges in the graph (for homophily).
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

#Homophily
def homopily(n, nr_red_nodes, nr_blue_nodes, nr_edges, nr_mixed_edges):
    #Calculate expected number and measure real number of mixed edges.
    probability_mixed_edge = 2*nr_red_nodes*nr_blue_nodes/(n*(n-1))
    actual_fraction_mixed_edges = nr_mixed_edges / nr_edges
    # print(f'Probability of a mixed edge: {probability_mixed_edge}, actual fraction of mixed edges: {actual_fraction_mixed_edges}.')
    return probability_mixed_edge, actual_fraction_mixed_edges

def has_illusions(G, k=None):
    #Check whether graph G has majority weak majority illusion, assuming that it is colored in blue and red.
    # If G is a regular graph, the degree can be indicated by k.    

    colors = nx.get_node_attributes(G, "color")
    nr_blue = list(colors.values()).count('blue')
    if nr_blue > G.number_of_nodes()/2:
        global_majority = 'blue'
    elif nr_blue < G.number_of_nodes()/2:
        global_majority = 'red'
    else: global_majority = 'tie'
    # print(f'Global majority: {global_majority}')

    nodes_under_illusion = {}
    for node in G.nodes():
        node_under_illusion = False
        if k:
            degree = k
        else:
            degree = G.degree(node)
        count_red = 0
        for nb in G.neighbors(node):
            if G.nodes[nb]['color'] == 'red':
                count_red += 1
        # print(f'Number of red neighbors of node {node}: {count_red}. Degree: {degree}, half degree: {degree/2}.')
        if count_red > degree/2: 
            local_majority = 'red'
        elif count_red == degree/2:
            local_majority = 'tie'
        else: # count_red < degree/2
            local_majority = 'blue'
        if local_majority == global_majority:
            node_under_illusion = 'false'
        elif local_majority == 'tie' or global_majority == 'tie':
            node_under_illusion = 'weak'
        else: 
            node_under_illusion = 'strict'
        nodes_under_illusion[node]=node_under_illusion
#     print('nodes under illusion: ' + str(nodes_under_illusion))
    illusions = list(nodes_under_illusion.values())
#     print(illusions)
    nr_str_ill_nodes = illusions.count('strict')
    nr_weak_ill_nodes = illusions.count('weak') + illusions.count('strict')
    global_tie = False
    if global_majority == 'tie':
        global_tie = True
    if nr_str_ill_nodes > G.number_of_nodes()/2:
#         print('The network has a majority-majority-illusion.')
        return 'mmi', 'mwmi', nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie
    elif nr_str_ill_nodes == G.number_of_nodes()/2:
        if nr_weak_ill_nodes > G.number_of_nodes()/2:
            return 'wmmi', 'mwmi' , nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie
        else: #nr weak <= |V|/2 but strict -> weak so: nr weak = |V|/2
            return 'wmmi', 'wmwmi' , nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie
    else: # nr strict < |V|/2 : no strict illusion
        if nr_weak_ill_nodes > G.number_of_nodes()/2:
            return 'no_strict', 'mwmi' , nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie
        elif nr_weak_ill_nodes == G.number_of_nodes()/2: 
            return 'no_strict', 'wmwmi' , nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie
        else:
            return 'no_strict', 'no_weak' , nr_str_ill_nodes, nr_weak_ill_nodes, nodes_under_illusion, global_tie

def save_data(data, min_n, max_n, step_n, nr_epochs, graph_type):
    path = 'C:/Users/P281866/Documents/PhD/Programming/Majority_Illusion/output/'
    filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + '_' + graph_type + '.csv'
    data.to_csv(path+filename, header=True, sep=';')

def add_dataframes(path, filename1, filename2, filename3):
    data1 = pd.read_csv(path + filename1, sep=';', index_col=0)
    data2 = pd.read_csv(path + filename2, sep=';', index_col=0)
    data3 = pd.concat([data1, data2], ignore_index=True)
    data3.to_csv(path + filename3, header=True, sep=';')


# Helper function to read strings as numbers
def to_float_list(x):
    if type(x) != str:
        # print(f' Type: {type(x)}, x: {x}')
        x = []
    if type(x) == str:
        x = x.replace("[", "").replace("]", "").split(', ')
        x = list(map(float, x))
    return x
    
    # Helper function to read strings as numbers
# def to_float_list(x):
#     if type(x) != str:
#         if type(x) ==  pd.core.series.Series :
#             print('series type')
#             x = x.array[0]
#             print(x)
#             x = x.replace("[", "").replace("]", "").split(', ')
#             x = list(map(float, x)) 
#         else: 
#             print(f' Type: {type(x)}, x: {x}')
#             print(' other type ')
#             x = []
#     if type(x) == str:
#         x = x.replace("[", "").replace("]", "").split(', ')
#         x = list(map(float, x))
#     return x