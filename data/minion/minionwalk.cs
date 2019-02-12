using UnityEngine;
using System.Collections;

public class minionwalk : MonoBehaviour
{
	private Animator animator;
	public GameObject pos;
	public plains home;
     	public int ms = 1;
	        // Use this for initialization
		//
	public int[,] pathmap = new int[500, 500];
	public int movedir = 10;
	public int movet = 0;
	public int movex = 0;
	public int movey = 0;
	void Start()
	{
		animator = this.GetComponent<Animator>();
   	}
		//                      
		// Update is called once per frame
	void Update()
	{                                   
		var vertical = Input.GetAxis("Vertical");
		var horizontal = Input.GetAxis("Horizontal");                      

		Transform t = pos.transform;
		Vector3 v = t.position;
		int x = (int)(Mathf.Round((v.x)*2))+250;
		int y = (int)(Mathf.Round((v.y)*2))+250;
		
		if (movedir == 0) {
			animator.SetInteger("dir", 0);
			transform.Translate(Vector3.down*ms*Time.deltaTime);
			movet++;
			if (movet > 7) {
				movedir = 10;
				movet = 0;
			}
		}
		if (movedir == 1) {
			animator.SetInteger("dir", 1);
			transform.Translate(Vector3.left*ms*Time.deltaTime);
			movet++;
			if (movet > 7) {
				movedir = 10;
				movet = 0;
			}
		}
		if (movedir == 2) {
			animator.SetInteger("dir", 2);
			transform.Translate(Vector3.up*ms*Time.deltaTime);
			movet++;
			if (movet > 7) {
				movedir = 10;
				movet = 0;
			}
		}
		if (movedir == 3) {
			animator.SetInteger("dir", 3);
			transform.Translate(Vector3.right*ms*Time.deltaTime);
			movet++;
			if (movet > 7) {
				movedir = 10;
				movet = 0;
			}
		}

		if (vertical > 0)
		{
			if (horizontal < 0) {
				animator.SetInteger("dir", 5);
				//if((home.ncons[x, y]!=0) && (home.scons[x, y+1]!=0)){
					transform.Translate(2*(Vector3.up*ms*Time.deltaTime)/3);
				//}
				//if((home.wcons[x, y]!=0) && (home.econs[x-1, y]!=0)){
					transform.Translate(2*(Vector3.left*ms*Time.deltaTime)/3);
			//	}
			}
			else if (horizontal > 0) {
				animator.SetInteger("dir", 6);
			//	if((home.ncons[x, y]!=0) && (home.scons[x, y+1]!=0)){
					transform.Translate(2*(Vector3.up*ms*Time.deltaTime)/3);
			//	}
			//	if((home.econs[x, y]!=0) && (home.wcons[x+1, y]!=0)){
					transform.Translate(2*(Vector3.right*ms*Time.deltaTime)/3);
			//	}
			}
			else {
			//	if((home.ncons[x, y]!=0) && (home.scons[x, y+1]!=0)) {
					animator.SetInteger("dir", 2);
					transform.Translate(Vector3.up*ms*Time.deltaTime);
			//	}
			}
		}
		else if (vertical < 0)
		{
                       if (horizontal < 0) {
				animator.SetInteger("dir", 7);
			//	if((home.scons[x, y]!=0) && (home.ncons[x, y-1]!=0)){
					transform.Translate(2*(Vector3.down*ms*Time.deltaTime)/3);
			//	}
			//	if((home.wcons[x, y]!=0) && (home.econs[x-1, y]!=0)){
					transform.Translate(2*(Vector3.left*ms*Time.deltaTime)/3);
			//	}
			}
			else if (horizontal > 0) {
				animator.SetInteger("dir", 8);
				transform.Translate(Vector3.down*ms*Time.deltaTime);
			//	if((home.scons[x, y]!=0) &&(home.ncons[x, y-1]!=0)){
					transform.Translate(2*(Vector3.down*ms*Time.deltaTime)/3);
			//	}
			//	if((home.econs[x, y]!=0) && (home.wcons[x+1, y]!=0)){
					transform.Translate(2*(Vector3.right*ms*Time.deltaTime)/3);
			//	}
			}
			else {
				animator.SetInteger("dir", 0);
			//	if((home.scons[x, y]!=0) && (home.ncons[x, y-1]!=0)){
					transform.Translate(Vector3.down*ms*Time.deltaTime);
			//	}
			}
		}
		else
		{
			if (horizontal < 0) {
				animator.SetInteger("dir", 1);
			//	if((home.wcons[x, y]!=0) && (home.econs[x-1, y]!=0)){
					transform.Translate(Vector3.left*ms*Time.deltaTime);
			//	}
			}
			else if (horizontal > 0) {
				animator.SetInteger("dir", 3);
			//	if((home.econs[x, y]!=0) && (home.wcons[x+1, y]!=0)){
					transform.Translate(Vector3.right*ms*Time.deltaTime);
			//	}
			}
			else {
				animator.SetInteger("dir", 4);
			}
		}
		if(((x-250) == movex) && ((y-250) == movey)) {
			movex = 0;
			movey = 0;
		}	
		if((movex != 0) || (movey != 0)) {
			move(movex, movey);
		}
	}

	public void move(float x, float y){
		int dir;
		float px = 2*pos.transform.position.x;
		float py = 2*pos.transform.position.y;
		
		float dirx = x - px;
		float diry = y - py;

		int done = 0;		

		if(Mathf.Abs(dirx) > Mathf.Abs(diry)) {
			if(dirx < 0) {
				dir = 1;
			}
			else {
				dir = 3;
			}
		}
		else{
			if(diry < 0) {
				dir = 2;
			}
			else {
				dir = 0;
			}
		}

		int cx = (int)Mathf.Round(x + 250);
		int cy = (int)Mathf.Round(y + 250);
		movex = cx;
		movey = cy;
		setpathmap(x+250, y+250, 1, (int)Mathf.Abs(px)+250, (int)Mathf.Abs(py)+250, done);

		if((cx < 490) && (cx > 1) && (cy < 490) && (cy > 1)) {
			int best = 1000;
			int path = -1;
			if((best >= pathmap[cx, cy-1]) && (pathmap[cx, cy-1]!=0)){
				path = 0;
				best = pathmap[cx, cy-1];
			}
			if((best >= pathmap[cx-1, cy]) && (pathmap[cx-1, cy]!=0)){
				if (best == pathmap[cx-1, cy]) {
					if (dir == 1) {
						path = 1;
						best = pathmap[cx-1, cy];
					}
				}
				else {
					path = 1;
					best = pathmap[cx-1, cy];
				}	
			}
			if((best >= pathmap[cx, cy+1]) && (pathmap[cx, cy+1]!=0)){
				if(best == pathmap[cx, cy+1]) {
					if(dir == 2) {
						path = 2;
						best = pathmap[cx, cy+1];
					}
				}
				else {
					path = 2;
					best = pathmap[cx, cy+1];
				}
			}
			if((best >= pathmap[cx+1, cy]) && (pathmap[cx+1, cy]!=0)){
				if(best == pathmap[cx+1, cy]) {
					if (dir == 3) {
						path = 3;
						best = pathmap[cx+1, cy];
					}
				}
				else {
					path = 3;
					best = pathmap[cx+1, cy];
				}
			}
			movedir = path;
		}
	}

	public int setpathmap(float x, float y, int i, int px, int py, int done){
		int cx = (int)(Mathf.Round(x));
		int cy = (int)(Mathf.Round(y));

		if ((cx > 490) || (cx < 0) || (cy > 490) || (cy < 0)) {
			return done;
		}

		if (done == 1) {
			return done;
		}

		if ((x == px) && (y == py)) {
			done = 1;
		}

		if (pathmap[cx, cy] > 0)
			return done;

		pathmap[cx, cy] = i;
		//if((home.scons[cx, cy]!=0) && (home.ncons[cx, cy-1]!=0)) {
			done = setpathmap(cx, cy-1, i+1, px, py, done);
		//}
		//if((home.wcons[cx, cy]!=0) && (home.econs[cx-1, cy]!=0)) {
			done = setpathmap(cx-1, cy, i+1, px, py, done);
	//	}
	//	if((home.ncons[cx, cy]!=0) && (home.scons[cx, cy+1]!=0)) {
			done = setpathmap(cx, cy+1, i+1, px, py, done);
	//	}
	//	if((home.econs[cx, cy]!=0) && (home.wcons[cx+1, cy]!=0)) {
			done = setpathmap(cx+1, cy, i+1, px, py, done);
	//	}

		return done;
	}
}
