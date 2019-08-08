data Tree a = Nil
            | Node a (Tree a) (Tree a)

depthTree :: Tree a -> Int
depthTree (Nil) = 0
depthTree (Node x leftNode rightNode) = theDepth
      where theDepth = maximum (lDepth+1, rDepth+1)
            lDepth = depthTree leftNode
            rDepth = depthTree rightNode


                -- int maxDepth(struct node* node)  
                -- { 
                --    if (node==NULL)  
                --        return 0; 
                --    else 
                --    { 
                --        /* compute the depth of each subtree */
                --        int lDepth = maxDepth(node->left); 
                --        int rDepth = maxDepth(node->right); 
                  
                --        /* use the larger one */
                --        if (lDepth > rDepth)  
                --            return(lDepth+1); 
                --        else return(rDepth+1); 
                --    } 
                -- }  