
import matplotlib.pyplot as plt
import seaborn as sns
import math
import numpy as np

# https://www.youtube.com/watch?v=NHwXkvwSd7E
def make_heatmaps(data):

    fig, axes = plt.subplots(nrows=2, ncols=3, figsize = (20, 20))
    # plt.title('Average fraction of nodes under majority illusion')

    plot1_data = data[['n', 'p_edge', 'avg_frac_nodes_ill']]
    plot1_data = plot1_data.groupby(['n', 'p_edge']).mean()
    plot1_data = plot1_data.reset_index()
    pivot_0_0 = plot1_data.pivot('n', 'p_edge', 'avg_frac_nodes_ill')
    sns.heatmap(pivot_0_0, annot=True, linewidths=.5, square=True,ax=axes[0,0], cbar=False)
    axes[0,0].set_title('Average fraction of nodes under majority illusion,\n comparing n and p_edge')

    plot2_data = data[['n', 'p_blue', 'avg_frac_nodes_ill']]
    plot2_data = plot2_data.groupby(['n', 'p_blue']).mean()
    plot2_data = plot2_data.reset_index()
    pivot_0_1 = plot2_data.pivot('n', 'p_blue', 'avg_frac_nodes_ill')
    sns.heatmap(pivot_0_1, annot=True, linewidths=.5, square=True,ax=axes[0,1], cbar=False)
    axes[0,1].set_title('Average fraction of nodes under majority illusion,\n comparing n and p_blue')


    plot3_data = data[['p_edge', 'p_blue', 'avg_frac_nodes_ill']]
    plot3_data = plot3_data.groupby(['p_edge', 'p_blue']).mean()
    plot3_data = plot3_data.reset_index()
    pivot_0_2 = plot3_data.pivot('p_edge', 'p_blue', 'avg_frac_nodes_ill')
    sns.heatmap(pivot_0_2, annot=True, linewidths=.5, square=True,ax=axes[0,2], cbar=False)
    axes[0,2].set_title('Average fraction of nodes under majority illusion,\n comparing p_edge and p_blue')


    plot1_data = data[['n', 'p_edge', 'frac_runs_mmi']]
    plot1_data = plot1_data.groupby(['n', 'p_edge']).mean()
    plot1_data = plot1_data.reset_index()
    pivot_0_3 = plot1_data.pivot('n', 'p_edge', 'frac_runs_mmi')
    sns.heatmap(pivot_0_3, annot=True, linewidths=.5, square=True,ax=axes[1,0], cbar=False)
    axes[1,0].set_title('Fraction of runs with majority majority illusion,\n comparing n and p_edge')

    plot2_data = data[['n', 'p_blue', 'frac_runs_mmi']]
    plot2_data = plot2_data.groupby(['n', 'p_blue']).mean()
    plot2_data = plot2_data.reset_index()
    pivot_0_4 = plot2_data.pivot('n', 'p_blue', 'frac_runs_mmi')
    sns.heatmap(pivot_0_4, annot=True, linewidths=.5, square=True,ax=axes[1,1], cbar=False)
    axes[1,1].set_title('Fraction of runs with majority majority illusion,\n comparing n and p_blue')


    plot3_data = data[['p_edge', 'p_blue', 'frac_runs_mmi']]
    plot3_data = plot3_data.groupby(['p_edge', 'p_blue']).mean()
    plot3_data = plot3_data.reset_index()
    pivot_0_5 = plot3_data.pivot('p_edge', 'p_blue', 'frac_runs_mmi')
    sns.heatmap(pivot_0_5, annot=True, linewidths=.5, square=True,ax=axes[1,2], cbar=False)
    axes[1,2].set_title('Fraction of runs with majority majority illusion,\n comparing p_edge and p_blue')


def save_data(data, min_n, max_n, step_n, nr_epochs, graph_type):
    path = 'C:/Users/P281866/Documents/PhD/Programming/Majority_Illusion/output/'
    filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + '_' + graph_type + '.csv'
    data.to_csv(path+filename, header=True, sep=';')


#TODO: line width according to standard deviation?
def draw_plots(data, min_n, max_n, step_n, nr_epochs, p_blue_values):
    # Draws plots with the average fraction of nodes with an illusion and the fraction of networks with a majority-majority illusion.
    node_numbers = range(min_n, max_n+1, step_n)
    nr_of_figures = math.ceil((max_n+1-min_n)/step_n)

    if nr_of_figures <= 6:
        plt.figure(1, figsize=(int(nr_of_figures*6), 4)).tight_layout(w_pad=2)
    else:
        plt.figure(1, figsize=(math.ceil(nr_of_figures/2)*6, 8)).tight_layout(w_pad=2)
    # plt.figure(1, figsize=(15, 12))
    # plt.axes().set_prop_cycle(cycler('color', plt.cm.tab20c_r.colors))
    colors = plt.cm.Reds(np.linspace(0,1,20))
    ax = plt.axes().get_anchor()
    plt.subplots_adjust(hspace = 0.5, top=0.8) #top ensures that the title does not overlap the subtitles
    plt.suptitle("Average fraction of nodes with an illusion (over "+ str(nr_epochs) + " random networks per set of param.)", fontsize=14, y=0.95)
    
    if nr_of_figures <= 6:
        plt.figure(2, figsize=(int(nr_of_figures*6), 4)).tight_layout(w_pad=2)
    else:
        plt.figure(2, figsize=(math.ceil(nr_of_figures/2)*6, 8)).tight_layout(w_pad=2)
    plt.subplots_adjust(hspace=0.5, top = 0.8)
    plt.suptitle("Fraction of networks (over "+ str(nr_epochs) + " runs) with a majority-majority-illusion", fontsize=14, y=0.95)

    axnr = 0
    
    for n in node_numbers: 
        axnr += 1
        n_data = data[data['n']==n] # Look only at the data for the current n 
        # print(n_data)
        # print(n_data.mean())
        for p_blue in p_blue_values:
            pb_n_data = n_data[n_data['p_blue']==p_blue] # Look only at the data for the current value of p_blue
            # print(pb_n_data)

            #Plot the average fraction of nodes under illusion against p_edge in the first plot, with a color indicating p_blue
            plt.figure(1)
            if nr_of_figures <= 6:
                ax = plt.subplot(1, nr_of_figures, axnr)
            else: 
                ax = plt.subplot(2, math.ceil(nr_of_figures/2), axnr)
            ax.set_title('n = ' + str(n))

            #Make a visible colorscale: 
            range_p_blue = p_blue_values[-1]-p_blue_values[0]
            # print(f'range_p_blue: {range_p_blue}')
            # color_nr = int((p_blue/range_p_blue)*10+2)
            color_nr = int(((p_blue/range_p_blue)-(p_blue_values[0]/range_p_blue))*10+2)
            # print(f'colornumber: {color_nr}')
            
            pb_n_data.plot(x='p_edge', y='avg_frac_nodes_ill', ax = ax, label = 'p_blue: '+ str(p_blue), c=colors[color_nr], legend = True )

            #Plot the fraction of runs with a majority majority illusion against p_edge in the second plot, with a color indicating p_blue
            plt.figure(2)
            if nr_of_figures <= 6:
                ax = plt.subplot(1, nr_of_figures, axnr)
            else: 
                ax = plt.subplot(2, math.ceil(nr_of_figures/2), axnr)
            ax.set_title('n = ' + str(n))
            pb_n_data.plot(x='p_edge', y='frac_runs_mmi', ax = ax, label = 'p_blue: '+ str(p_blue), c=colors[color_nr], legend = True )
        # print(n_data.sem()) #to get the standard deviation from the mean

    if nr_epochs>= 50: #Save the figure only for long runs
        path = 'C:/Users/P281866/Documents/PhD/Programming/Majority_Illusion/output/'
        plt.figure(1)
        filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + 'avg_frac_nodes_ill.png'
        plt.savefig(path+filename)
        plt.figure(2)
        filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + 'frac_runs_mmi.png'
        plt.savefig(path+filename)



def draw_plots_WS(data, min_n, max_n, step_n, nr_epochs, p_blue_values):
    # Draws plots with the average fraction of nodes with an illusion and the fraction of networks with a majority-majority illusion.
    node_numbers = range(min_n, max_n+1, step_n)
    nr_of_figures = math.ceil((max_n+1-min_n)/step_n)

    if nr_of_figures <= 6:
        plt.figure(1, figsize=(int(nr_of_figures*6), 4)).tight_layout(w_pad=2)
    else:
        plt.figure(1, figsize=(math.ceil(nr_of_figures/2)*6, 8)).tight_layout(w_pad=2)
    # plt.figure(1, figsize=(15, 12))
    # plt.axes().set_prop_cycle(cycler('color', plt.cm.tab20c_r.colors))
    colors = plt.cm.Reds(np.linspace(0,1,20))
    ax = plt.axes().get_anchor()
    plt.subplots_adjust(hspace = 0.5, top=0.8) #top ensures that the title does not overlap the subtitles
    plt.suptitle("Average fraction of nodes with an illusion (over "+ str(nr_epochs) + " random networks per set of param.)", fontsize=14, y=0.95)
    
    if nr_of_figures <= 6:
        plt.figure(2, figsize=(int(nr_of_figures*6), 4)).tight_layout(w_pad=2)
    else:
        plt.figure(2, figsize=(math.ceil(nr_of_figures/2)*6, 8)).tight_layout(w_pad=2)
    plt.subplots_adjust(hspace=0.5, top = 0.8)
    plt.suptitle("Fraction of networks (over "+ str(nr_epochs) + " runs) with a majority-majority-illusion", fontsize=14, y=0.95)

    axnr = 0
    
    for n in node_numbers: 
        axnr += 1
        n_data = data[data['n']==n] # Look only at the data for the current n 
        # print(n_data)
        # print(n_data.mean())
        for p_blue in p_blue_values:
            pb_n_data = n_data[n_data['p_blue']==p_blue] # Look only at the data for the current value of p_blue
            # print(pb_n_data)

            #Plot the average fraction of nodes under illusion against beta in the first plot, with a color indicating p_blue
            plt.figure(1)
            if nr_of_figures <= 6:
                ax = plt.subplot(1, nr_of_figures, axnr)
            else: 
                ax = plt.subplot(2, math.ceil(nr_of_figures/2), axnr)
            ax.set_title('n = ' + str(n))

            #Make a visible colorscale: 
            range_p_blue = p_blue_values[-1]-p_blue_values[0]
            # print(f'range_p_blue: {range_p_blue}')
            # color_nr = int((p_blue/range_p_blue)*10+2)
            color_nr = int(((p_blue/range_p_blue)-(p_blue_values[0]/range_p_blue))*10+2)
            # print(f'colornumber: {color_nr}')
            
            pb_n_data.plot(x='beta', y='avg_frac_nodes_ill', ax = ax, label = 'p_blue: '+ str(p_blue), c=colors[color_nr], legend = True )

            #Plot the fraction of runs with a majority majority illusion against beta in the second plot, with a color indicating p_blue
            plt.figure(2)
            if nr_of_figures <= 6:
                ax = plt.subplot(1, nr_of_figures, axnr)
            else: 
                ax = plt.subplot(2, math.ceil(nr_of_figures/2), axnr)
            ax.set_title('n = ' + str(n))
            pb_n_data.plot(x='beta', y='frac_runs_mmi', ax = ax, label = 'p_blue: '+ str(p_blue), c=colors[color_nr], legend = True )
        # print(n_data.sem()) #to get the standard deviation from the mean

    if nr_epochs>= 50: #Save the figure only for long runs
        path = 'C:/Users/P281866/Documents/PhD/Programming/Majority_Illusion/output/'
        plt.figure(1)
        filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + 'avg_frac_nodes_ill.png'
        plt.savefig(path+filename)
        plt.figure(2)
        filename = 'minn' + str(min_n) + '_maxn' + str(max_n) + '_step' + str(step_n) + '_epochs' + str(nr_epochs) + 'frac_runs_mmi.png'
        plt.savefig(path+filename)
