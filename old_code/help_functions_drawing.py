# Nicer drawings:
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np     
import pandas as pd
from numpy import cos, pi, sin, sqrt
from matplotlib.patches import Circle

def node_table(G, group_by=None, sort_by=None):
    node_table = []
    node_index = []
    for n, d in G.nodes(data=True):
        node_table.append(d)
        node_index.append(n)
    df = pd.DataFrame(data=node_table, index=node_index)
    df = group_and_sort(df, group_by, sort_by)
    return df

def node_colors(G, color_by, nt):
    """Return pandas Series of node colors."""
    if color_by:
        color_dict = nx.get_node_attributes(G, "color")
        return color_dict
    # print('nt:'+ str(nt))
    return pd.Series(["blue"] * len(nt), name="color_by", index=nt.index)

def group_and_sort(node_table, group_by = None, sort_by = None):
    """Group and sort a node table."""
    sort_criteria = []
    if group_by:
        sort_criteria.append(group_by)
    if sort_by:
        sort_criteria.append(sort_by)
    if sort_criteria:
        node_table = node_table.sort_values(sort_criteria)
    return node_table

def circos_radius(n_nodes: int, node_radius: float = 1.0):
    """
    Automatically computes the origin-to-node centre radius of the Circos plot
    using the triangle equality sine rule.

    a / sin(A) = b / sin(B) = c / sin(C)

    :param n_nodes: the number of nodes in the plot.
    :param node_radius: the radius of each node.
    :returns: Origin-to-node centre radius.
    """
    A = 2 * np.pi / n_nodes  # noqa
    B = (np.pi - A) / 2  # noqa
    a = 2 * node_radius
    return a * np.sin(B) / np.sin(A)

def to_cartesian(r, theta):
    """
    Converts polar r, theta to cartesian x, y.
    """
    x = r * cos(theta)
    y = r * sin(theta)
    return x, y

def item_theta(itemlist, item):
    """
    Maps node to an angle in radians.

    :param itemlist: Item list from the graph.
    :param item: The item of interest. Must be in the itemlist.
    :returns: theta -- the angle of the item in radians.
    """
    assert len(itemlist) > 0, "itemlist must be a list of items."
    assert item in itemlist, "item must be inside itemlist."

    i = itemlist.index(item)
    theta = i * 2 * np.pi / len(itemlist)

    return theta

def layout_func_circos(nt, group_by = None, sort_by = None, radius = None):
    """Circos plot node layout."""
    # print('layout function circos plot')
    pos = dict()
    nt = group_and_sort(nt, group_by, sort_by)
    nodes = list(nt.index)
    if radius is None:
        radius = circos_radius(len(nodes))
    if group_by:
        for grp, df in nt.groupby(group_by):
            for node, data in df.iterrows():
                x, y = to_cartesian(r=radius, theta=item_theta(nodes, node))
                pos[node] = np.array([x, y])
    else:
        for node, data in nt.iterrows():
            x, y = to_cartesian(r=radius, theta=item_theta(nodes, node))
            pos[node] = np.array([x, y])
    return pos

def node_glyphs(nt, pos, node_color):
    """Draw circos glyphs to the matplotlib axes object."""
    patches = dict()
    for r, d in nt.iterrows():
        kw = {"fc": node_color[r], "radius":0.5, "zorder": 3, "label": 'x'}
        c = Circle(xy=pos[r], **kw)
        patches[r] = c
    return pd.Series(patches)

def emphasize_illusion_nodes(nt, pos, node_color, illusions):
    """Draw circos glyphs to the matplotlib axes object."""
    emph_patches = dict()
    for r, d in nt.iterrows():
        if illusions[r]:
            kw = {"fc": node_color[r], "radius":0.7, "zorder": 2, "label": 'x', "fill": False}
            c = Circle(xy=pos[r], **kw)
            emph_patches[r] = c
    return pd.Series(emph_patches)


def draw_graph(G, illusions = None):
    color_by = "color"
    nt = node_table(G)
    node_color = node_colors(G, color_by, nt)
    pos = layout_func_circos(nt, group_by = 'color', sort_by = 'color')
    # print(f'pos: {pos}')

    patches = node_glyphs(nt, pos, node_color)
    # print(f'patches: {patches.head()}')
    if illusions:
        emph_patches = emphasize_illusion_nodes(nt, pos, node_color, illusions)
    
    ax = plt.gca()
    width = 0.6*G.number_of_nodes()
#     ax.set(xlim=(-5, 5), ylim=(-5, 5))
    ax.set(xlim=((-1)*width, width), ylim=((-1)*width, width))


    #     pos: {2: array([2., 0.]), 5: array([1.        , 1.73205081]), 0: array([-1.        ,  1.73205081]), 1: array([-2.0000000e+00,  2.4492936e-16]), 4: array([-1.        , -1.73205081]), 3: array([ 1.        , -1.73205081])}
    for edge in G.edges:
        #edge[0] and edge[1] are the nodes connected to this edge
        node_0 = edge[0]
        node_1 = edge[1]
        x = [pos[node_0][0], pos[node_1][0]]
        y = [pos[node_0][1], pos[node_1][1]]
        edge_color = G.edges[edge]['color']
        plt.plot(x,y, color=edge_color)

    if illusions:
        for emph_patch in emph_patches:
            ax.add_patch(emph_patch)    
    for patch in patches:
        ax.add_patch(patch)


    for node in G.nodes:
        label = ax.annotate(node, xy=pos[node], fontsize=10, ha="center", color = 'white')
    plt.axis('off')
    plt.show()
    return plt

#     #Save figure:
#     foldername = 'C:\\Users\\P281866\\Documents\\PhD\\Majority-illusion-programming\\Generated_figures\\'
#     filename = foldername + 'n-' + str(n) + '_and_k-' + str(k)
#     plt.savefig(filename)

def color_randomly(graph, p_blue=0.5, half=False):
    #Color a graph randomly in red and blue, 
    # if half=True, with one (if odd n) or two (if even n) more red than blue nodes,
    # otherwise at random with a probability p for blue.

    n = graph.number_of_nodes()
    print(f'n: {n}')
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
    print(f'length randomised_colours: {len(randomised_colours)}')
    for node in graph.nodes():
        graph.nodes[node]['color'] = randomised_colours[node]

    return

def color_edges(graph):
    # Colour the edges of a graph with coloured nodes according to the colour of the nodes.
    edge_colors ={}
    for edge in graph.edges():
        #edge[0] and edge[1] are the nodes connected to this edge
        if graph.nodes[edge[0]]['color'] == graph.nodes[edge[1]]['color']: # If the two nodes have the same colour
            graph.edges[edge]['color'] = graph.nodes[edge[0]]['color']
            edge_colors[edge] = graph.nodes[edge[0]]['color']
        else:
            graph.edges[edge]['color'] = 'black'
            edge_colors[edge] = 'black'
        #or?:
        # nx.set_edge_attributes(graph, edge_colors, name="color")
    return
