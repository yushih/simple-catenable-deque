open Deque



/*
let render_suspend = s => {
  let CatDeque.Susp(_labda, repr) = s;
  let render_repr = r => {
    switch (r) {
    |CatDeque.Lazy_empty => <div>{text("Empty")}</div>
    |CatDeque.Lazy_tail_force =>  
    }
  };
  <div> 
    {text("||")}
  </div>
}
*/

/*
let render_suspend = s => {
  let CatDeque.Susp(lambda, repr) = s;
  let v = lambda();
  <div>
     {text("middle")}
     {render_queue(v)}
  </div>
}
*/
let text = ReasonReact.string;

let render_deque = q => {
  let rec render_cat : 'a.(('a=>ReasonReact.reactElement, CatDeque.cat('a))=>ReasonReact.reactElement) = (render_elem, q) => {
    let render_base = q => { // box already rendered by caller
      let elems = Array.map(render_elem, q);
      <div className="no-box">
        {if (Array.length(elems)==0) {
           text("[]");
         } else {
           ReasonReact.array(elems)}
         }
      </div>
    };
    

    switch (q) {
    |CatDeque.Shallow(arr) =>
      <div className="render-box content-member">
        <div className="title-box shallow">
          {text("Shallow")}
        </div>
        <div className="content-box">
          {render_base(arr)}
        </div>
      </div>
    |CatDeque.Deep(l, m, r) =>
      let CatDeque.Susp(lambda, repr) = m;
      let v = lambda();
      <div className="render-box content-member">
        <div className="title-box deep">
          {text("Deep")}
        </div>
        <div className="content-box">
          {render_base(l)}
          {render_cat(render_base, v)}
          {render_base(r)}
        </div>
      </div>
    }//switch
  };

  let render_number = i => <div className="int-box content-member">{text(string_of_int(i))}</div>;
  render_cat(render_number, q)
}

type state = {
  current_deq: CatDeque.cat(int),
  next_cons: string
}

type action = 
    | NextConsChange(string)
    | Cons
;

let component = ReasonReact.reducerComponent("Main")

let make = _children => {
    ...component,

    initialState: ()=> 
      /*
         let deq = ref(CatDeque.empty)

         for (i in 1 to 10) {
         deq := CatDeque.cons(i, deq^)
         }

         deq := CatDeque.concat(deq^, deq^)
       */
    {
      current_deq: CatDeque.empty,
      next_cons: "0"
    },

    reducer: (action, state) => 
      switch (action) {
      |NextConsChange(value) => ReasonReact.Update({...state, next_cons: value})
      |Cons => {
        let next_cons = int_of_string(state.next_cons);
        ReasonReact.Update(
          {...state, 
           current_deq: CatDeque.cons(next_cons, state.current_deq),
           next_cons: string_of_int(next_cons+1)
          })
        }
       },//switch (action)

    render: self =>
      <div>
        <input 
          type_="text" 
          value=(self.state.next_cons)
          onChange=(e => self.send(NextConsChange(ReactEvent.Form.target(e)##value)))></input>
        <button onClick=(_e => self.send(Cons))>{text("cons")}</button>
        <div className="root-box">
          {render_deque(self.state.current_deq)}
        </div>
      </div>
};
